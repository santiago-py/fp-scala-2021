package Week1

object recursion_assigment1 {

//  Exercise 1: Pascal's Triangle,
//  calculate the value in the column, row of pascals triangle 
  
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) return 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  
//  Exercise 2: Parentheses Balancing 
//  write recursive function that checks whether the input string is balanced in parenthesis
  
  def balance(chars: List[Char]): Boolean = {
    def balanced(chars: List[Char], count: Int): Boolean =
      if (chars.isEmpty) return count == 0
      else if (count < 0) return false
      else if (chars.head == '(') balanced(chars.tail, count + 1)
      else if (chars.head == ')') balanced(chars.tail, count - 1)
      else balanced(chars.tail, count)

    balanced(chars, 0)
  }

  @main def run(): Unit = {
    println(pascal(1,3))
    println(balance("(())".toList))
  }

}

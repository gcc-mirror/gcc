/* PR tree-optimization/17512

   We used to try to fold "(char) (X ^ Y)", where '^' is
   TRUTH_XOR_EXPR into ((char) X ^ (char) Y), creating TRUTH_XOR_EXPR
   with its operands being of type char, which is invalid.  */

char
foo (int p)
{
  int q = p;
  return (p != 0) == (p == q);
}

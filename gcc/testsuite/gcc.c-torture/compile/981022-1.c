/* This tests a combination of two gcc extensions.  Omitting the middle
   operand of ?: and using ?: as an lvalue.  */
int x, y;

int main ()
{
  (x ?: y) = 0;
  return 0;
}

/* { dg-do compile { target ia64-*-* } } */
/* { dg-options "-O2" } */

int
foo (int x, int y)
{
  if (y == 0)
    {
      register long r8 asm ("r8");
      register long r15 asm ("r15") = 1;
      long retval;
      __asm __volatile ("foo" : "=r" (r8), "=r" (r15) : "1" (r15));
      retval = r8;
      y = retval;
    }

  {
    register long r8 asm ("r8");
    register long r15 asm ("r15") = 2;
    long retval;
    register long _out1 asm ("out1") = x;
    register long _out0 asm ("out0") = y;
    __asm __volatile ("foo"
		      : "=r" (r8), "=r" (r15) , "=r" (_out0), "=r" (_out1)
		      : "1" (r15) , "2" (_out0), "3" (_out1));
    retval = r8;
    return retval;
  }
}

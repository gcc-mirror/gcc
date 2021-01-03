/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all" } */

long double ret_x87 (void)
{
  return 1.1L;
}

/* { dg-final { scan-assembler-times "fldz" 7 } } */
/* { dg-final { scan-assembler-times "fstp\[ \t\]+%st\\(0\\)" 7 } } */

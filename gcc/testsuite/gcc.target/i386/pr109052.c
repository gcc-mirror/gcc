/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mfpmath=both -msse2" } */

double foo (double a)
{
  double tmp = a;
  asm ("" : "+t" (tmp));
  return a * tmp;
}

/* { dg-final { scan-assembler-times "movsd\t" 1 } } */

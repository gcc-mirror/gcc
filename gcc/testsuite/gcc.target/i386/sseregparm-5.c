/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
/* { dg-require-effective-target ilp32 } */

/* Make sure we know that mysinfp returns in %xmm0.  */

double __attribute__((sseregparm)) mysin(void);
double __attribute__((sseregparm)) (*mysinfp)(void) = mysin;
double bar(double x)
{
  return mysinfp();
}

/* { dg-final { scan-assembler "fldl" } } */

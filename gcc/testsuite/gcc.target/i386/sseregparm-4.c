/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */
/* { dg-require-effective-target ia32 } */

/* Make sure we know that mysinfp returns in %xmm0.  */

double __attribute__((sseregparm)) mysin(double x);
double __attribute__((sseregparm)) (*mysinfp)(double) = mysin;
double bar(double x)
{
  return mysinfp(x);
}

/* { dg-final { scan-assembler "fldl" } } */

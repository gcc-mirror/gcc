/* { dg-do run { target { powerpc*-*-linux* } } } */
/* { dg-options "-O2 -ftree-vectorize" } */

/* Verify it can run successfully, especially on Power10 and later.   */

#define _GNU_SOURCE
#include <fenv.h>

__attribute__ ((noipa)) void
div (double *d, double *s, int n)
{
  for (; n; n--, d++, s++)
    *d = *d / *s;
}

int main()
{
  double d[] = {1,2,3,4,5,6,7,8,9,10,11};
  double s[] = {11,10,9,8,7,6,5,4,3,2,1};

  feenableexcept(FE_DIVBYZERO|FE_INVALID);
  div(d, s, 11);

  int i;
  for (i = 0; i < 11; i++)
    __builtin_printf(" %f", d[i]);

  __builtin_printf("\n");

  return 0;
}

/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-O2 -fdump-ipa-icf"  } */
/* { dg-add-options c99_runtime } */

#include <complex.h>

static double test;

__attribute__ ((noinline))
double f1(void)
{
  double complex z1 = 1.0 + 3.0 * I;
  double complex z2 = 1.0 - 4.0 * I;

  unsigned a = 123;
  unsigned b = 321;

  if (a & b)
    return 1.2f;

  if(cimag(z1) > 1)
    return 1.0f;

  test = cimag(z1) + 2;

  return cimag(z1 + z2);
}

__attribute__ ((noinline))
double f2(void)
{
  double complex z1 = 1.0 + 3.0 * I;
  double complex z2 = 1.0 - 4.0 * I;

  unsigned a = 123;
  unsigned b = 321;

  if (a & b)
    return 1.2f;

  if(cimag(z1) > 1)
    return 1.0f;

  test = cimag(z1) + 2;

  return cimag(z1 + z2);
}

int main()
{
  return 1;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:f1->f2" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */

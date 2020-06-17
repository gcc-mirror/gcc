/* { dg-do compile { target c99_runtime } } */
/* { dg-options "-O0 -fipa-icf -fdump-ipa-icf-optimized"  } */

#include <complex.h>

typedef _Complex float COMPLEX_FLOAT;

__attribute__ ((noinline))
static float real_part(COMPLEX_FLOAT a)
{
  return *(float*)(&a);
}

__attribute__ ((noinline))
static float real_part_2(COMPLEX_FLOAT a)
{
  return ((float*)(&a))[0];
}

int main()
{
  COMPLEX_FLOAT f = 1.0f + _Complex_I;

  float r1 = real_part(f);
  float r2 = real_part_2(f);

  return r1 - r2;
}

/* { dg-final { scan-ipa-dump "Semantic equality hit:real_part/\[0-9+\]+->real_part_2/\[0-9+\]+" "icf"  } } */
/* { dg-final { scan-ipa-dump "Equal symbols: 1" "icf"  } } */

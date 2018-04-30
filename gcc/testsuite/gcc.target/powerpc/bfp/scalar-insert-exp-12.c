/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mcpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

double
insert_exponent (unsigned long long int *significand_p,
		 unsigned long long int *exponent_p)
{
  unsigned long long int significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

#define BIAS_FOR_DOUBLE_EXP 1023

int
main ()
{
  unsigned long long int significand_1 = 0x18000000000000LL;
  unsigned long long int significand_2 = 0x1a000000000000LL;
  unsigned long long int exponent_1 = 62 + BIAS_FOR_DOUBLE_EXP;
  unsigned long long int exponent_2 = 49 + BIAS_FOR_DOUBLE_EXP;

  double x = (double) (0x1800ULL << 50);
  double z = (double) (0x1a00ULL << 37);


  if (insert_exponent (&significand_1, &exponent_1) != x)
    abort ();
  if (insert_exponent (&significand_2, &exponent_2) != z)
    abort ();
  return 0;
}

/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */
/* { dg-require-effective-target has_arch_ppc64 } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

double
insert_exponent (double *significand_p,
		 unsigned long long int *exponent_p)
{
  double significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

#define BIAS_FOR_DOUBLE_EXP 1023

int
main ()
{
  unsigned long long int significand_1 = 0x11000000000000LL;
  unsigned long long int significand_2 = 0x11010000000000LL;
  unsigned long long int exponent_1 = 62 + BIAS_FOR_DOUBLE_EXP;
  unsigned long long int exponent_2 = 49 + BIAS_FOR_DOUBLE_EXP;

  double *significand_1_ptr = (double *) &significand_1;
  double *significand_2_ptr = (double *) &significand_2;


  double x = (double) (0x1100LL << 50);
  double z = (double) (0x1101LL << 37);

  if (insert_exponent (significand_1_ptr, &exponent_1) != x)
    abort ();
  if (insert_exponent (significand_2_ptr, &exponent_2) != z)
    abort ();
  return 0;
}

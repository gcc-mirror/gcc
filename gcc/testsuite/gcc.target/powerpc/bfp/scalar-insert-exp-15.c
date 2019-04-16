/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9" } */

/* This test should succeed only on 64-bit configurations.  */
#include <altivec.h>
#include <stdlib.h>

__ieee128
insert_exponent (__ieee128 *significand_p,
		 unsigned long long int *exponent_p)
{
  __ieee128 significand = *significand_p;
  unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

#define BIAS_FOR_QUAD_EXP 16383

int
main ()
{
  /* most-significant bit @13, shift it to position 113 */
  unsigned __int128 significand_1 = ((unsigned __int128) 0x1100) << 100;
  unsigned __int128 significand_2 = ((unsigned __int128) 0x1101) << 100;
  unsigned long long int exponent_1 = 126 + BIAS_FOR_QUAD_EXP;
  unsigned long long int exponent_2 = 124 + BIAS_FOR_QUAD_EXP;

  __ieee128 *significand_1_ptr = (__ieee128 *) &significand_1;
  __ieee128 *significand_2_ptr = (__ieee128 *) &significand_2;

  __ieee128 x = (__ieee128) (((__int128) 0x1100LL) << 114);
  __ieee128 z = (__ieee128) (((__int128) 0x1101LL) << 112);

  if (insert_exponent (significand_1_ptr, &exponent_1) != x)
    abort ();
  if (insert_exponent (significand_2_ptr, &exponent_2) != z)
    abort ();
  return 0;
}

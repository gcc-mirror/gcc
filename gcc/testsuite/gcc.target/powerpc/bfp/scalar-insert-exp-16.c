/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -save-temps" } */

#include <altivec.h>
#include <stdlib.h>

#ifdef DEBUG
#include <stdio.h>
#endif

__ieee128
insert_exponent (vector unsigned __int128 *significand_p,
		 vector unsigned long long int *exponent_p)
{
  vector unsigned __int128 significand = *significand_p;
  vector unsigned long long int exponent = *exponent_p;

  return scalar_insert_exp (significand, exponent);
}

__ieee128
insert_exponent2 (unsigned __int128 significand,
		 unsigned long long int exponent)
{
   return scalar_insert_exp (significand, exponent);
}

int
main ()
{
  __ieee128 val_ieee128, result_ieee128, exp_result_ieee128;
  unsigned __int128 val_int128;
  unsigned long long int  val_ull;
  union conv128_t
   {
     __ieee128 val_ieee128;
     vector unsigned __int128 val_vint128;
     vector unsigned long long int  val_vull;
  } result, exp_result, significand;

  vector unsigned long long int exponent;

  /* Scalar argument test */
  val_ieee128 = 0xFEDCBA9876543210ULL;
  val_ull = 0x5678;
#ifdef _BIG_ENDIAN
  exp_result.val_vull[1] = 0xfedcba9876543210;  
  exp_result.val_vull[0] = 0x5678000000000000ULL;
#else
  exp_result.val_vull[0] = 0xfedcba9876543210;  
  exp_result.val_vull[1] = 0x5678000000000000ULL;
#endif
  result_ieee128 = insert_exponent2 (val_ieee128, val_ull);

  if (result_ieee128 != exp_result.val_ieee128)
#ifdef DEBUG
    {
      result.val_ieee128 = result_ieee128;
      printf("Scalar argument ERROR:\n");
      printf(" val_ieee128 = 0x%llx %llx\n",
	     result.val_vull[1], result.val_vull[0]);
      printf(" exp_val_ieee128 = 0x%llx %llx\n",
	     exp_result.val_vull[1], exp_result.val_vull[0]);
    }
#else
    abort ();
#endif

  /* Vector argument test */
  significand.val_vull[0] = 0xFEDCBA9876543210ULL;
  significand.val_vull[1] = 0x7FFF12345678ABCDULL;  /* positive value */

  exponent[0] = 0x5678;
  exponent[1] = 0x1234;

#ifdef _BIG_ENDIAN
  exp_result.val_vull[0] = 0xD678BA9876543210ULL;
  exp_result.val_vull[1] = 0x7FFF12345678ABCDULL;
#else
  exp_result.val_vull[0] = 0xFEDCBA9876543210ULL;
  exp_result.val_vull[1] = 0x123412345678ABCDULL;
#endif
  result.val_ieee128 = insert_exponent(&significand.val_vint128, &exponent);
  
  if (result.val_ieee128 != exp_result.val_ieee128)
#ifdef DEBUG
    {
      printf("Vector argument ERROR:\n");
      printf(" result = 0x%llx %llx\n",
	     result.val_vull[1], result.val_vull[0]);
      printf(" exp_result = 0x%llx %llx\n",
	     exp_result.val_vull[1], exp_result.val_vull[0]);
    }
#else
    abort ();
#endif

}

/* Check that the expected insert exponent instruction is generated. */
/* { dg-final { scan-assembler-times {\mxsiexpqp\M} 2 } } */

/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -save-temps" } */

#include <altivec.h>
#include <stdlib.h>

#if DEBUG
#include <stdio.h>
#endif

vector unsigned __int128
get_significand (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_sig_to_vec(source);
}

int
main ()
{
  #define NOT_ZERO_OR_DENORMAL  0x1000000000000

  union conv128_t
   {
     __ieee128 val_ieee128;
     unsigned long long int val_ull[2];
     unsigned __int128  val_uint128;
     vector unsigned __int128  val_vuint128;
  } source, result, exp_result;
  
  /* Result is not zero or denormal.  */
#ifdef _BIG_ENDIAN
  exp_result.val_ull[0] = 0x00056789ABCDEF0ULL | NOT_ZERO_OR_DENORMAL;
  exp_result.val_ull[1] = 0x123456789ABCDEFULL;
#else
  exp_result.val_ull[1] = 0x00056789ABCDEF0ULL | NOT_ZERO_OR_DENORMAL;
  exp_result.val_ull[0] = 0x123456789ABCDEFULL;
#endif
  source.val_uint128 = 0x923456789ABCDEF0ULL;
  source.val_uint128 = (source.val_uint128 << 64) | 0x123456789ABCDEFULL;

  /* Note, bits[0:14] are set to 0, bit[15] is 0 if the input was zero or
     Denormal, 1 otherwise.  */
  result.val_vuint128 = get_significand (&source.val_ieee128);

  if ((result.val_ull[0] != exp_result.val_ull[0])
      || (result.val_ull[1] != exp_result.val_ull[1]))
#if DEBUG
    {
      printf("result[0] = 0x%llx; exp_result[0] = 0x%llx\n",
	     result.val_ull[0], exp_result.val_ull[0]);
      printf("result[1] = 0x%llx; exp_result[1] = 0x%llx\n",
	     result.val_ull[1], exp_result.val_ull[1]);
    }
#else
    abort();
#endif
  return 0;
}

/* Check that the expected extract significand instruction is generated. */
/* { dg-final { scan-assembler-times {\mxsxsigqp\M} 1 } } */

/* { dg-do run { target { powerpc*-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-mdejagnu-cpu=power9 -save-temps" } */

#include <altivec.h>
#include <stdlib.h>

#if DEBUG
#include <stdio.h>
#endif

vector unsigned long long int
get_exponents (__ieee128 *p)
{
  __ieee128 source = *p;

  return scalar_extract_exp_to_vec (source);
}

int
main ()
{
  vector unsigned long long int result, exp_result;
  union conv128_t
   {
     __ieee128 val_ieee128;
     __int128  val_int128;
  } source;
  
#ifdef _BIG_ENDIAN
  exp_result[1] = 0x0ULL;
  exp_result[0] = 0x1234ULL;
#else
  exp_result[0] = 0x0ULL;
  exp_result[1] = 0x1234ULL;
#endif
  source.val_int128 = 0x923456789ABCDEF0ULL;
  source.val_int128 = (source.val_int128 << 64) | 0x123456789ABCDEFULL;

  result = get_exponents (&source.val_ieee128);

  if ((result[0] != exp_result[0]) || (result[1] != exp_result[1]))
#if DEBUG
    {
      printf("result[0] = 0x%llx; exp_result[0] = 0x%llx\n",
	     result[0], exp_result[0]);
      printf("result[1] = 0x%llx; exp_result[1] = 0x%llx\n",
	     result[1], exp_result[1]);
    }
#else
    abort();
#endif
  return 0;
}

/* Check that the expected extract exponent instruction is generated. */
/* { dg-final { scan-assembler-times {\mxsxexpqp\M} 1 } } */

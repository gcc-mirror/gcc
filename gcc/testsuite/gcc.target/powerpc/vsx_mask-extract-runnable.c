/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -save-temps" } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mvextractbm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextracthm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextractwm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextractdm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvextractqm\M} 1 } } */


#define DEBUG 0

#if DEBUG
#include <stdio.h>
#include <stdlib.h>
#endif
#include <altivec.h>

void abort (void);

int main ()
{
  unsigned long long result, expected_result;
  vector unsigned char vbc_bi_src;
  vector unsigned short vbc_hi_src;
  vector unsigned int vbc_wi_src;
  vector unsigned long long vbc_di_src;
  vector __uint128_t vbc_qi_src;
  
/* vextractbm */
  vbc_bi_src[0] = 0xFF;
  vbc_bi_src[1] = 0xFF;
  vbc_bi_src[2] = 0x0;
  vbc_bi_src[3] = 0x0;
  vbc_bi_src[4] = 0x0;
  vbc_bi_src[5] = 0x0;
  vbc_bi_src[6] = 0xFF;
  vbc_bi_src[7] = 0xFF;
  vbc_bi_src[8] = 0xFF;
  vbc_bi_src[9] = 0xFF;
  vbc_bi_src[10] = 0xFF;
  vbc_bi_src[11] = 0xFF;
  vbc_bi_src[12] = 0xFF;
  vbc_bi_src[13] = 0x0;
  vbc_bi_src[14] = 0xFF;
  vbc_bi_src[15] = 0xFF;

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  expected_result = 0b1100001111111011;
#else
  expected_result = 0b1101111111000011;
#endif

  result = vec_extractm (vbc_bi_src);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: char vec_extractm(%d) ", vbc_bi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

    /* vextracthm */
  vbc_hi_src[0] = 0xFFFF;
  vbc_hi_src[1] = 0xFFFF;
  vbc_hi_src[2] = 0x0;
  vbc_hi_src[3] = 0x0;
  vbc_hi_src[4] = 0x0;
  vbc_hi_src[5] = 0x0;
  vbc_hi_src[6] = 0xFFFF;
  vbc_hi_src[7] = 0xFFFF;

  expected_result = 0b11000011;

  result = vec_extractm (vbc_hi_src);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: short vec_extractm(%d) ", vbc_hi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractwm */
  vbc_wi_src[0] = 0xFFFFFFFF;
  vbc_wi_src[1] = 0xFFFFFFFF;
  vbc_wi_src[2] = 0x0;
  vbc_wi_src[3] = 0x0;

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
  expected_result = 0b1100;
#else
  expected_result = 0b0011;
#endif

  result = vec_extractm (vbc_wi_src);

  if (result != expected_result) {
#if DEBUG
    printf("ERROR: word vec_extractm(%d) ", vbc_wi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractdm */
  vbc_di_src[0] = 0xFFFFFFFFFFFFFFFF;
  vbc_di_src[1] = 0xFFFFFFFFFFFFFFFF;

  expected_result = 0b11;

  result = vec_extractm (vbc_di_src);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: double vec_extractm(%lld) ", vbc_di_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractqm */
  vbc_qi_src[0] = 0x1;
  vbc_qi_src[0] = vbc_qi_src[0] << 127;
  
  expected_result = 1;

  result = vec_extractm (vbc_qi_src);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: quad vec_extractm(arg) ");
    printf("result 0x%x does not match expected result = 0x%x\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  return 0;
}

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
  int i, num_elements;
  unsigned long long arg1;
  
  vector unsigned char  vbc_result_bi, vbc_expected_result_bi;
  vector unsigned short vbc_result_hi, vbc_expected_result_hi;
  vector unsigned int  vbc_result_wi, vbc_expected_result_wi;
  vector unsigned long long vbc_result_di, vbc_expected_result_di;
  vector __uint128_t vbc_result_qi, vbc_expected_result_qi;

  unsigned int result_wi, expected_result_wi;
  unsigned long long result, expected_result;
  const unsigned char mp=1;
  vector unsigned char vbc_bi_src;
  vector unsigned short vbc_hi_src;
  vector unsigned int vbc_wi_src;
  vector unsigned long long vbc_di_src;
  vector __uint128_t vbc_qi_src;
  
/* vextractbm */
  num_elements = 8;
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

  expected_result_wi = 0b1101111111000011;

  result_wi = vec_extractm (vbc_bi_src);
  
  if (result_wi != expected_result_wi) {
#if DEBUG
    printf("ERROR: short vec_extractm(%d) ", vbc_bi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

    /* vextracthm */
  num_elements = 8;
  vbc_hi_src[0] = 0xFFFF;
  vbc_hi_src[1] = 0xFFFF;
  vbc_hi_src[2] = 0x0;
  vbc_hi_src[3] = 0x0;
  vbc_hi_src[4] = 0x0;
  vbc_hi_src[5] = 0x0;
  vbc_hi_src[6] = 0xFFFF;
  vbc_hi_src[7] = 0xFFFF;

  expected_result_wi = 0b11000011;

  result_wi = vec_extractm (vbc_hi_src);
  
  if (result_wi != expected_result_wi) {
#if DEBUG
    printf("ERROR: short vec_extractm(%d) ", vbc_hi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractwm */
  num_elements = 4;
  vbc_wi_src[0] = 0xFFFFFFFF;
  vbc_wi_src[1] = 0xFFFFFFFF;
  vbc_wi_src[2] = 0x0;
  vbc_wi_src[3] = 0x0;

  expected_result_wi = 0b0011;

  result_wi = vec_extractm (vbc_wi_src);
  
  if (result_wi != expected_result_wi) {
#if DEBUG
    printf("ERROR: word vec_extractm(%d) ", vbc_wi_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractdm */
  num_elements = 2;
  vbc_di_src[0] = 0xFFFFFFFFFFFFFFFF;
  vbc_di_src[1] = 0xFFFFFFFFFFFFFFFF;

  expected_result_wi = 0b11;

  result_wi = vec_extractm (vbc_di_src);
  
  if (result_wi != expected_result_wi) {
#if DEBUG
    printf("ERROR: double vec_extractm(%lld) ", vbc_di_src);
    printf("result %llu does not match expected result = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vextractqm */
  num_elements = 1;
  vbc_qi_src[0] = 0x1;
  vbc_qi_src[0] = vbc_qi_src[0] << 127;
  
  expected_result_wi = 1;

  result_wi = vec_extractm (vbc_qi_src);
  
  if (result_wi != expected_result_wi) {
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

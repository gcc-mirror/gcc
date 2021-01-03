/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -save-temps" } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mvcntmbb\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcntmbh\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcntmbw\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvcntmbd\M} 1 } } */

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
  
  /* vcntmbb */
  num_elements = 16;
  vbc_bi_src[0] = 0xFF;
  vbc_bi_src[1] = 0xFF;
  vbc_bi_src[2] = 0x0;
  vbc_bi_src[3] = 0x0;
  vbc_bi_src[4] = 0x0;
  vbc_bi_src[5] = 0x0;
  vbc_bi_src[6] = 0xFF;
  vbc_bi_src[7] = 0xFF;
  vbc_bi_src[8] = 0x0;
  vbc_bi_src[9] = 0x0;
  vbc_bi_src[10] = 0x0;
  vbc_bi_src[11] = 0x0;
  vbc_bi_src[12] = 0x0;
  vbc_bi_src[13] = 0xFF;
  vbc_bi_src[14] = 0xFF;
  vbc_bi_src[15] = 0xFF;

  expected_result = 7;

  result = vec_cntm (vbc_bi_src, 1);
  /* Note count is put in bits[0:7], IBM numbering, of the 64-bit result */
  result = result >> (64-8);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: char vec_cntm(arg) ");
    printf("count %llu does not match expected count = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vcntmhb */
  num_elements = 8;
  vbc_hi_src[0] = 0xFFFF;
  vbc_hi_src[1] = 0xFFFF;
  vbc_hi_src[2] = 0x0;
  vbc_hi_src[3] = 0x0;
  vbc_hi_src[4] = 0x0;
  vbc_hi_src[5] = 0x0;
  vbc_hi_src[6] = 0xFFFF;
  vbc_hi_src[7] = 0xFFFF;

  expected_result = 4;

  result = vec_cntm (vbc_hi_src, 1);
  /* Note count is put in bits[0:6], IBM numbering, of the 64-bit result */
  result = result >> (64-7);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: short vec_cntm(arg) ");
    printf("count %llu does not match expected count = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vcntmwb */
  num_elements = 4;
  vbc_wi_src[0] = 0xFFFFFFFF;
  vbc_wi_src[1] = 0xFFFFFFFF;
  vbc_wi_src[2] = 0x0;
  vbc_wi_src[3] = 0x0;

  expected_result = 2;

  result = vec_cntm (vbc_wi_src, 1);
  /* Note count is put in bits[0:5], IBM numbering, of the 64-bit result */
  result = result >> (64-6);
  
  if (result != expected_result) {
#if DEBUG
    printf("ERROR: word vec_cntm(arg) ");
    printf("count %llu does not match expected count = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  /* vcntmdb */
  num_elements = 2;
  vbc_di_src[0] = 0xFFFFFFFFFFFFFFFFULL;
  vbc_di_src[1] = 0xFFFFFFFFFFFFFFFFULL;

  expected_result = 2;

  result = vec_cntm (vbc_di_src, 1);
  /* Note count is put in bits[0:4], IBM numbering, of the 64-bit result */
  result = result >> (64-5);

  if (result != expected_result) {
#if DEBUG
    printf("ERROR: double vec_cntm(arg) ");
    printf("count %llu does not match expected count = %llu\n",
	   result, expected_result);
#else
    abort();
#endif
  }

  return 0;
}

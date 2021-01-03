/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -O2 -save-temps" } */
/* { dg-require-effective-target power10_ok } */
/* { dg-require-effective-target int128 } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mvexpandbm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvexpandhm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvexpandwm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvexpanddm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvexpandqm\M} 1 } } */

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
  
  /* vexpandbm */
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

  vbc_expected_result_bi[0] = 0xFF;
  vbc_expected_result_bi[1] = 0xFF;
  vbc_expected_result_bi[2] = 0x0;
  vbc_expected_result_bi[3] = 0x0;
  vbc_expected_result_bi[4] = 0x0;
  vbc_expected_result_bi[5] = 0x0;
  vbc_expected_result_bi[6] = 0xFF;
  vbc_expected_result_bi[7] = 0xFF;
  vbc_expected_result_bi[8] = 0x0;
  vbc_expected_result_bi[9] = 0x0;
  vbc_expected_result_bi[10] = 0x0;
  vbc_expected_result_bi[11] = 0x0;
  vbc_expected_result_bi[12] = 0x0;
  vbc_expected_result_bi[13] = 0xFF;
  vbc_expected_result_bi[14] = 0xFF;
  vbc_expected_result_bi[15] = 0xFF;

  vbc_result_bi = vec_expandm (vbc_bi_src);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_bi[i] != vbc_expected_result_bi[i]) {
#if DEBUG
      printf("ERROR: char vec_expandm(arg) ");
      printf("element %d, 0x%x does not match expected value = 0x%x\n",
	     i, vbc_result_bi[i], vbc_expected_result_bi[i]);
#else
    abort();
#endif
    }
  }

  /* vexpandhm */
  num_elements = 8;
  vbc_hi_src[0] = 0x0;
  vbc_hi_src[1] = 0xFFFF;
  vbc_hi_src[2] = 0x0;
  vbc_hi_src[3] = 0xFFFF;
  vbc_hi_src[4] = 0x0;
  vbc_hi_src[5] = 0x0;
  vbc_hi_src[6] = 0xFFFF;
  vbc_hi_src[7] = 0xFFFF;

  vbc_expected_result_hi[0] = 0x0;
  vbc_expected_result_hi[1] = 0xFFFF;
  vbc_expected_result_hi[2] = 0x0;
  vbc_expected_result_hi[3] = 0xFFFF;
  vbc_expected_result_hi[4] = 0x0;
  vbc_expected_result_hi[5] = 0x0;
  vbc_expected_result_hi[6] = 0xFFFF;
  vbc_expected_result_hi[7] = 0xFFFF;

  vbc_result_hi = vec_expandm (vbc_hi_src);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_hi[i] != vbc_expected_result_hi[i]) {
#if DEBUG
      printf("ERROR: short vec_expandm(arg) ");
      printf("element %d, 0x%x does not match expected value = 0x%x\n",
	     i, vbc_result_hi[i], vbc_expected_result_hi[i]);
#else
    abort();
#endif
    }
  }
  
  /* vexpandwm */
  num_elements = 4;
  vbc_wi_src[0] = 0x0;
  vbc_wi_src[1] = 0xFFFFFFFF;
  vbc_wi_src[2] = 0x0;
  vbc_wi_src[3] = 0xFFFFFFFF;

  vbc_expected_result_wi[0] = 0x0;
  vbc_expected_result_wi[1] = 0xFFFFFFFF;
  vbc_expected_result_wi[2] = 0x0;
  vbc_expected_result_wi[3] = 0xFFFFFFFF;

  vbc_result_wi = vec_expandm (vbc_wi_src);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_wi[i] != vbc_expected_result_wi[i]) {
#if DEBUG
      printf("ERROR: int vec_expandm(arg) ");
      printf("element %d, 0x%x does not match expected value = 0x%x\n",
	     i, vbc_result_wi[i], vbc_expected_result_wi[i]);
#else
    abort();
#endif
    }
  }
  
  /* vexpanddm */
  num_elements = 2;
  vbc_di_src[0] = 0x0;
  vbc_di_src[1] = 0xFFFFFFFFFFFFFFFFULL;

  vbc_expected_result_di[0] = 0x0;
  vbc_expected_result_di[1] = 0xFFFFFFFFFFFFFFFFULL;

  vbc_result_di = vec_expandm (vbc_di_src);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_di[i] != vbc_expected_result_di[i]) {
#if DEBUG
      printf("ERROR: double vec_expandm(arg) ");
      printf("element %d, 0x%llx does not match expected value = 0x%llx\n",
	     i, vbc_result_di[i], vbc_expected_result_di[i]);
#else
    abort();
#endif
    }
  }
  
  /* vexpandqm */
  num_elements = 1;
  vbc_qi_src[0] = 0x0;

  vbc_expected_result_qi[0] = 0x0;

  vbc_result_qi = vec_expandm (vbc_qi_src);
  
  if (vbc_result_qi[0] != vbc_expected_result_qi[0]) {
#if DEBUG
    printf("ERROR: quad vec_expandm(arg) ");
    printf("element %d, 0x%x does not match expected value = 0x%x\n",
	   0, vbc_result_qi[i], vbc_expected_result_qi[i]);
#else
    abort();
#endif
  }

  return 0;
}

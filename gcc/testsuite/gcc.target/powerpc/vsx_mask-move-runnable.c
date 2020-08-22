/* { dg-do run } */
/* { dg-options "-mcpu=power10 -O2" } */
/* { dg-require-effective-target power10_hw } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mmtvsrbm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrhm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrwm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrdm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrqm\M} 1 } } */
/* { dg-final { scan-assembler-times {\mmtvsrbmi\M} 2 } } */

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
  
 /* mtvsrbmi */
  num_elements = 16;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_bi[i] = 0x0;

  vbc_expected_result_bi[0] = 0xFF;
  vbc_expected_result_bi[2] = 0xFF;

  vbc_result_bi = vec_genbm(5);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_bi[i] != vbc_expected_result_bi[i]) {
#if DEBUG
      printf("ERROR: vec_genbm(const 5) ");
      printf("element %d equals 0x%x does not match expected_result = 0x%x",
	     i, vbc_result_bi[i], vbc_expected_result_bi[i]);
      printf("\n\n");
#else
    abort();
#endif
    }
  }

  /* mtvsrbm */
  num_elements = 16;
  /* -O2 should generate mtvsrbmi as argument will fit in 6-bit field. */
  arg1 = 3;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_bi[i] = 0x0;

  vbc_expected_result_bi[1] = 0xFF;
  vbc_expected_result_bi[0] = 0xFF;

  vbc_result_bi = vec_genbm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_bi[i] != vbc_expected_result_bi[i]) {
#if DEBUG
      printf("ERROR: vec_genbm(%d) ", arg1);
      printf("element %d equals 0x%x does not match expected_result = 0x%x",
	     i, vbc_result_bi[i], vbc_expected_result_bi[i]);
      printf("\n\n");
#else
    abort();
#endif
    }
  }

  num_elements = 16;
  /* Should generate mtvsrbm as argument will not fit in 6-bit field. */
  arg1 = 0xEA;   // 234 decimal
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_bi[i] = 0x0;

  vbc_expected_result_bi[7] = 0xFF;
  vbc_expected_result_bi[6] = 0xFF;
  vbc_expected_result_bi[5] = 0xFF;
  vbc_expected_result_bi[3] = 0xFF;
  vbc_expected_result_bi[1] = 0xFF;

  vbc_result_bi = vec_genbm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_bi[i] != vbc_expected_result_bi[i]) {
#if DEBUG
      printf("ERROR: vec_genbm(%d) ", arg1);
      printf("element %d equals 0x%x does not match expected_result = 0x%x",
	     i, vbc_result_bi[i], vbc_expected_result_bi[i]);
      printf("\n\n");
#else
    abort();
#endif
    }
  }

  /* mtvsrhm */
  num_elements = 8;
  arg1 = 5;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_hi[i] = 0x0;

  vbc_expected_result_hi[2] = 0xFFFF;
  vbc_expected_result_hi[0] = 0xFFFF;

  vbc_result_hi = vec_genhm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_hi[i] != vbc_expected_result_hi[i]) {
#if DEBUG
      printf("ERROR: vec_genhm(%d) ", arg1);
      printf("element %d equals 0x%x does not match expected_result = 0x%x",
	     i, vbc_result_hi[i], vbc_expected_result_hi[i]);
      printf("\n\n");
#else
    abort();
#endif
    }
  }

  /* mtvsrwm */
  num_elements = 4;
  arg1 = 7;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_wi[i] = 0x0;

  vbc_expected_result_wi[2] = 0xFFFFFFFF;
  vbc_expected_result_wi[1] = 0xFFFFFFFF;
  vbc_expected_result_wi[0] = 0xFFFFFFFF;

  vbc_result_wi = vec_genwm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_wi[i] != vbc_expected_result_wi[i]) {
#if DEBUG
      printf("ERROR: vec_genwm(%d) ", arg1);
      printf("element %d equals 0x%x does not match expected_result = 0x%x",
	     i, vbc_result_wi[i], vbc_expected_result_wi[i]);
      printf("\n\n");
#else
    abort();
#endif
    }
  }

  /* mtvsrdm */
  num_elements = 2;
  arg1 = 1;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_di[i] = 0x0;

  vbc_expected_result_di[1] = 0x0;
  vbc_expected_result_di[0] = 0xFFFFFFFFFFFFFFFF;

  vbc_result_di = vec_gendm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_di[i] != vbc_expected_result_di[i]) {
#if DEBUG
      printf("ERROR: vec_gendm(%d) ", arg1);
      printf("element %d equals 0x%llx does not match expected_result = ",
	     i, vbc_result_di[i]);
      printf("0x%llx\n\n", vbc_expected_result_di[i]);
#else
    abort();
#endif
    }
  }

  /* mtvsrqm */
  num_elements = 1;
  arg1 = 1;
  
  for (i = 0; i<num_elements; i++)
    vbc_expected_result_qi[i] = 0x0;

  vbc_expected_result_qi[0] = 0xFFFFFFFFFFFFFFFFULL;
  vbc_expected_result_qi[0] = (vbc_expected_result_qi[0] << 64)
    | 0xFFFFFFFFFFFFFFFFULL;

  vbc_result_qi = vec_genqm(arg1);
  
  for (i = 0; i<num_elements; i++) {
    if (vbc_result_qi[i] != vbc_expected_result_qi[i]) {
#if DEBUG
      printf("ERROR: vec_genqm(%d) ", arg1);
      printf("element %d equals 0x%llx does not match expected_result = ",
	     i, vbc_result_qi[i]);
      printf("0x%llx\n\n", vbc_expected_result_qi[i]);
#else
    abort();
#endif
    }
  }

  return 0;
}

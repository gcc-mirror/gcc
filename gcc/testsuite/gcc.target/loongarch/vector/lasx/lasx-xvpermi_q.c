/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lasxintrin.h>

int
main ()
{
  __m256i __m256i_op0, __m256i_op1, __m256i_op2, __m256i_out, __m256i_result;
  __m256 __m256_op0, __m256_op1, __m256_op2, __m256_out, __m256_result;
  __m256d __m256d_op0, __m256d_op1, __m256d_op2, __m256d_out, __m256d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long*)& __m256i_op0[3]) = 0x7fe37fe3001d001d;
  *((unsigned long*)& __m256i_op0[2]) = 0x7fff7fff7fff0000;
  *((unsigned long*)& __m256i_op0[1]) = 0x7fe37fe3001d001d;
  *((unsigned long*)& __m256i_op0[0]) = 0x7fff7fff7fff0000;
  *((unsigned long*)& __m256i_op1[3]) = 0x7575757575757575;
  *((unsigned long*)& __m256i_op1[2]) = 0x7575757575757575;
  *((unsigned long*)& __m256i_op1[1]) = 0x7575757575757575;
  *((unsigned long*)& __m256i_op1[0]) = 0x7575757575757575;
  *((unsigned long*)& __m256i_result[3]) = 0x7fe37fe3001d001d;
  *((unsigned long*)& __m256i_result[2]) = 0x7fff7fff7fff0000;
  *((unsigned long*)& __m256i_result[1]) = 0x7fe37fe3001d001d;
  *((unsigned long*)& __m256i_result[0]) = 0x7fff7fff7fff0000;
  __m256i_out = __lasx_xvpermi_q (__m256i_op0, __m256i_op1, 0x22);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long*)& __m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_op0[2]) = 0x000000000019001c;
  *((unsigned long*)& __m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_op0[0]) = 0x000000000019001c;
  *((unsigned long*)& __m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_op1[2]) = 0x00000000000001fe;
  *((unsigned long*)& __m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_op1[0]) = 0x00000000000001fe;
  *((unsigned long*)& __m256i_result[3]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_result[2]) = 0x000000000019001c;
  *((unsigned long*)& __m256i_result[1]) = 0x0000000000000000;
  *((unsigned long*)& __m256i_result[0]) = 0x00000000000001fe;
  __m256i_out = __lasx_xvpermi_q (__m256i_op0, __m256i_op1, 0x31);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long*)& __m256i_op0[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long*)& __m256i_op0[2]) = 0x00ff00ff00ff00ff;
  *((unsigned long*)& __m256i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long*)& __m256i_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long*)& __m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long*)& __m256i_op1[2]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long*)& __m256i_op1[0]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long*)& __m256i_result[2]) = 0xffff0000ffff0000;
  *((unsigned long*)& __m256i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long*)& __m256i_result[0]) = 0x00ff00ff00ff00ff;
  __m256i_out = __lasx_xvpermi_q (__m256i_op0, __m256i_op1, 0x02);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

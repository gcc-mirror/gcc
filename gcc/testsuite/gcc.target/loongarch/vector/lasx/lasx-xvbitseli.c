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

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00000000000004fb;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000004fb;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0xef);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0xcd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffd10000006459;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000441000000004;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000004;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000040400000104;
  *((unsigned long *)&__m256i_op1[3]) = 0xdb801b6d0962003f;
  *((unsigned long *)&__m256i_op1[2]) = 0xdb8a3109fe0f0024;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000007fff01ffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xdb8e209d0cce025a;
  *((unsigned long *)&__m256i_result[3]) = 0x88888a6d0962002e;
  *((unsigned long *)&__m256i_result[2]) = 0xdb8a3109fe0f0020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000007fff01fffb;
  *((unsigned long *)&__m256i_result[0]) = 0xdb8e20990cce025a;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x88);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000002b902b3e;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000002b902b3e;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000002a102a3a;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x000000002a102a3a;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0xd9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_op0[2]) = 0xf0f0f0f0f0f0f0f0;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_op0[0]) = 0xf0f0f0f0f0f0f0f0;
  *((unsigned long *)&__m256i_op1[3]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_op1[2]) = 0xf0f0f0f0f0f0f0f0;
  *((unsigned long *)&__m256i_op1[1]) = 0x00000000f0f0f0f0;
  *((unsigned long *)&__m256i_op1[0]) = 0xf0f0f0f0f0f0f0f0;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000090909090;
  *((unsigned long *)&__m256i_result[2]) = 0x9090909090909090;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000090909090;
  *((unsigned long *)&__m256i_result[0]) = 0x9090909090909090;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x95);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x5555555555555555;
  *((unsigned long *)&__m256i_op0[2]) = 0x5555555555555555;
  *((unsigned long *)&__m256i_op0[1]) = 0x5555555555555555;
  *((unsigned long *)&__m256i_op0[0]) = 0x5555555555555555;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4545454545454545;
  *((unsigned long *)&__m256i_result[2]) = 0x4545454545454545;
  *((unsigned long *)&__m256i_result[1]) = 0x4545454545454545;
  *((unsigned long *)&__m256i_result[0]) = 0x4545454545454545;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x4d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_op0[2]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_op0[1]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_op0[0]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_op1[3]) = 0x21bb481000ff0000;
  *((unsigned long *)&__m256i_op1[2]) = 0x01bf481000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x21bb481000ff0000;
  *((unsigned long *)&__m256i_op1[0]) = 0x01bf481000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xb1b3b1b1b1b7b1b1;
  *((unsigned long *)&__m256i_result[2]) = 0xb1b7b1b1b1b1b1b1;
  *((unsigned long *)&__m256i_result[1]) = 0xb1b3b1b1b1b7b1b1;
  *((unsigned long *)&__m256i_result[0]) = 0xb1b7b1b1b1b1b1b1;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0xb7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[2]) = 0xc03fc03fc03fc03f;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000000000003f;
  *((unsigned long *)&__m256i_op0[0]) = 0xc03fc03fc03fc03f;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000002d;
  *((unsigned long *)&__m256i_result[2]) = 0xc02dc02dc02dc02d;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000002d;
  *((unsigned long *)&__m256i_result[0]) = 0xc02dc02dc02dc02d;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0xed);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x60600000ffff0000;
  *((unsigned long *)&__m256i_result[2]) = 0x6060000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x60600000ffff0000;
  *((unsigned long *)&__m256i_result[0]) = 0x6060000000000000;
  __m256i_out = __lasx_xvbitseli_b (__m256i_op0, __m256i_op1, 0x60);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

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
  *((unsigned long *)&__m256i_op0[2]) = 0x00007ffffffff7ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x49d8080067f4f81f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00007f00fffff7ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xd8490849f467f867;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0xb7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0xdb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0x95);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffb3b4;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffff5ffff4738;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffb3b4;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffff5ffff4738;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0xee);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0x2f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0x6f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_b (__m256i_op0, 0x23);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00007ffffffff7ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x49d8080067f4f81f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fff7fff7ffff7ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x080008000800f81f;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0xa8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_op0[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_op0[1]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_op0[0]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_result[3]) = 0xc5c4c5c5c5c5c5c5;
  *((unsigned long *)&__m256i_result[2]) = 0xc5c545c545c545c5;
  *((unsigned long *)&__m256i_result[1]) = 0xc5c4c5c5c5c5c5c5;
  *((unsigned long *)&__m256i_result[0]) = 0xc5c545c545c545c5;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0x3d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0xf7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0x3a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffff0000;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0xa7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_op0[2]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_op0[1]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_op0[0]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[3]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[2]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[1]) = 0xff1cff1cff1cff1c;
  *((unsigned long *)&__m256i_result[0]) = 0xff1cff1cff1cff1c;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0xdc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffff0020;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff8001ffff0001;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffff0020;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff8001ffff0001;
  *((unsigned long *)&__m256i_result[3]) = 0xffff000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffff8001ffff8001;
  *((unsigned long *)&__m256i_result[1]) = 0xffff000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffff8001ffff8001;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0x6e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0x9f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op0[2]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op0[0]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_result[3]) = 0x0002ffff00020002;
  *((unsigned long *)&__m256i_result[2]) = 0x04f504f104f504f5;
  *((unsigned long *)&__m256i_result[1]) = 0x0002ffff00020002;
  *((unsigned long *)&__m256i_result[0]) = 0x04f504f104f504f5;
  __m256i_out = __lasx_xvshuf4i_h (__m256i_op0, 0x65);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1e18000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x1e18000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x1e18000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x1e18000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1e1800001e180000;
  *((unsigned long *)&__m256i_result[2]) = 0x1e18000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x1e1800001e180000;
  *((unsigned long *)&__m256i_result[0]) = 0x1e18000000000000;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0xfe);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0x64);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_op0[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_op0[1]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_op0[0]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_result[3]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_result[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_result[1]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_result[0]) = 0x45c5c5c545c5c5c5;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0xb0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000f9f900;
  *((unsigned long *)&__m256i_op0[2]) = 0x79f9f9f900f9f9e0;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000f9f900;
  *((unsigned long *)&__m256i_op0[0]) = 0x79f9f9f900f9f900;
  *((unsigned long *)&__m256i_result[3]) = 0x00f9f90079f9f9f9;
  *((unsigned long *)&__m256i_result[2]) = 0x79f9f9f900000000;
  *((unsigned long *)&__m256i_result[1]) = 0x00f9f90079f9f9f9;
  *((unsigned long *)&__m256i_result[0]) = 0x79f9f9f900000000;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0x97);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000007aff7c00;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffd017d00;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000007aff7c00;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffd017d00;
  *((unsigned long *)&__m256i_result[3]) = 0x7aff7c0000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfd017d0000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x7aff7c0000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfd017d0000000000;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0xb3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_op0[2]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_op0[1]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_op0[0]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_result[3]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_result[2]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_result[1]) = 0xc3f0c3f0c3f0c3f0;
  *((unsigned long *)&__m256i_result[0]) = 0xc3f0c3f0c3f0c3f0;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0x3c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0xf4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffff81ff7d;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffff81ff7d;
  *((unsigned long *)&__m256i_result[3]) = 0xff81ff7dffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffff81ff7d;
  *((unsigned long *)&__m256i_result[1]) = 0xff81ff7dffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffff81ff7d;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0x28);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000002000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000020ff790020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000002000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000020ff790020;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvshuf4i_w (__m256i_op0, 0xa5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0101010183f95466;
  *((unsigned long *)&__m256i_op1[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op1[1]) = 0x01010101d58efe94;
  *((unsigned long *)&__m256i_op1[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010183f95466;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x01010101d58efe94;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0xa7);
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
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0xd9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[2]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[1]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[0]) = 0x00001fff00001fff;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffff80be0000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000f0f0002;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffff80be0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000f1002;
  *((unsigned long *)&__m256i_op1[3]) = 0x80000000ff800000;
  *((unsigned long *)&__m256i_op1[2]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x80000000ff800000;
  *((unsigned long *)&__m256i_op1[0]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[3]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[2]) = 0x80000000ff800000;
  *((unsigned long *)&__m256i_result[1]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[0]) = 0x80000000ff800000;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0xdb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000080000009;
  *((unsigned long *)&__m256i_op1[2]) = 0x43ef878780000009;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000080000009;
  *((unsigned long *)&__m256i_op1[0]) = 0x43ef878780000009;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x43ef878780000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x43ef878780000009;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0x36);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0x5a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0x5);
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
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fff00017fff0000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff00017fff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fff00017fff0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff00017fff0000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op1[2]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op1[0]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_result[3]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_result[2]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_result[1]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_result[0]) = 0x04f104f104f504ed;
  __m256i_out = __lasx_xvshuf4i_d (__m256i_op0, __m256i_op1, 0x7e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

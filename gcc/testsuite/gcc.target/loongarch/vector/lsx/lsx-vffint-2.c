/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lsxintrin.h>

int
main ()
{
  __m128i __m128i_op0, __m128i_op1, __m128i_op2, __m128i_out, __m128i_result;
  __m128 __m128_op0, __m128_op1, __m128_op2, __m128_out, __m128_result;
  __m128d __m128d_op0, __m128d_op1, __m128d_op2, __m128d_out, __m128d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long *)&__m128i_op0[1]) = 0x03ff03ff03ff03ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x438ff81ff81ff820;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4f8000004f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f8000004f800000;
  *((unsigned long *)&__m128d_result[1]) = 0x43d3e0000013e000;
  *((unsigned long *)&__m128d_result[0]) = 0x43d3e0000013e000;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xbff0000000000000;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0674c8868a74fc80;
  *((unsigned long *)&__m128i_op0[0]) = 0xfdce8003090b0906;
  *((unsigned long *)&__m128d_result[1]) = 0x4399d3221a29d3f2;
  *((unsigned long *)&__m128d_result[0]) = 0xc3818bffe7b7a7b8;
  __m128d_out = __lsx_vffint_d_l (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff00ff;
  *((int *)&__m128_result[3]) = 0x4b7f00ff;
  *((int *)&__m128_result[2]) = 0x4b7f00ff;
  *((int *)&__m128_result[1]) = 0x4b7f00ff;
  *((int *)&__m128_result[0]) = 0x4b7f00ff;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000401000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000004;
  *((int *)&__m128_result[3]) = 0x40800000;
  *((int *)&__m128_result[2]) = 0x4b800000;
  *((int *)&__m128_result[1]) = 0x47800080;
  *((int *)&__m128_result[0]) = 0x40800000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x47000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x76f424887fffffff;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x3f800000;
  *((int *)&__m128_result[1]) = 0x4eede849;
  *((int *)&__m128_result[0]) = 0x4f000000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_op0[0]) = 0xa352bfac9269e0aa;
  *((int *)&__m128_result[3]) = 0xce23d33d;
  *((int *)&__m128_result[2]) = 0x4edd53ea;
  *((int *)&__m128_result[1]) = 0xceb95a81;
  *((int *)&__m128_result[0]) = 0xcedb2c3f;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x3f800000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000003ff8;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x467fe000;
  __m128_out = __lsx_vffint_s_w (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff80000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0xbf800000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xcf000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x5eff0000;
  *((int *)&__m128_result[2]) = 0x5eff0000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000000e3;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfda9b23a624082fd;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffff0000;
  *((int *)&__m128_result[3]) = 0x43630000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xdc159371;
  *((int *)&__m128_result[0]) = 0x4f7fff00;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000040;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x42800000;
  *((int *)&__m128_result[0]) = 0x42800000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000100;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000100;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x43800000;
  *((int *)&__m128_result[0]) = 0x43800000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op1[0]) = 0x001effae001effae;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x59f7fd70;
  *((int *)&__m128_result[0]) = 0x59f7fd70;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000ef0000000003b;
  *((int *)&__m128_result[3]) = 0x577fff00;
  *((int *)&__m128_result[2]) = 0x577fff00;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x596f0000;
  __m128_out = __lsx_vffint_s_l (__m128i_op0, __m128i_op1);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}

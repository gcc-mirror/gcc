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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x54feed87bc3f2be1;
  *((unsigned long *)&__m128i_op0[0]) = 0x8064d8f6a494afcb;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1e801ffc7fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffe003c1f0077;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffff0074230438;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff0000000438;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0x2);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000800800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000800800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000004000000000;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfff5fff4002ffff5;
  *((unsigned long *)&__m128i_op1[1]) = 0xaa858644fb8b3d49;
  *((unsigned long *)&__m128i_op1[0]) = 0x18499e2cee2cc251;
  *((unsigned long *)&__m128i_result[1]) = 0x8644000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xaed495f03343a685;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7505443065413aed;
  *((unsigned long *)&__m128i_op0[0]) = 0x0100d6effefd0498;
  *((unsigned long *)&__m128i_op1[1]) = 0x7505443065413aed;
  *((unsigned long *)&__m128i_op1[0]) = 0x0100d6effefd0498;
  *((unsigned long *)&__m128i_result[1]) = 0xb71289fdfbea3f69;
  *((unsigned long *)&__m128i_result[0]) = 0x4e17c2ffb4851a40;
  __m128i_out = __lsx_vmul_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfc01fcfefc02fdf7;
  *((unsigned long *)&__m128i_op0[0]) = 0xfe00fcfffe01fd01;
  *((unsigned long *)&__m128i_op1[1]) = 0xfc01fd1300000001;
  *((unsigned long *)&__m128i_op1[0]) = 0xfe00fd1400010000;
  *((unsigned long *)&__m128i_result[1]) = 0xc72ef153fc02fdf7;
  *((unsigned long *)&__m128i_result[0]) = 0xca31bf15fd010000;
  __m128i_out = __lsx_vmul_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000001;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xc000c000c000ff81;
  *((unsigned long *)&__m128i_op1[1]) = 0x5d5d5d5d5d5d5d5d;
  *((unsigned long *)&__m128i_op1[0]) = 0x5d5d5d5d5d5d0000;
  *((unsigned long *)&__m128i_result[1]) = 0xa2a2a2a3a2a2a2a3;
  *((unsigned long *)&__m128i_result[0]) = 0xc605c000aedd0000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xacc8c794af2caf01;
  *((unsigned long *)&__m128i_op0[0]) = 0xa91e2048938c40f0;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xeeb1e4f43c3763f3;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff5a6fe3d7;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000021e79364;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000718ea657431b;
  *((unsigned long *)&__m128i_result[1]) = 0x000000006ca193ec;
  *((unsigned long *)&__m128i_result[0]) = 0x00008e72b5b94cad;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_op1[0]) = 0x467f6080467d607f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000001000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000001000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x007f008000ea007f;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xc000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0042003e0042002f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001fffc0001fffc;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffe0004fffe0004;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc1bdceee242070db;
  *((unsigned long *)&__m128i_op0[0]) = 0xe8c7b756d76aa478;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3f433212dce09025;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf359f359f359f359;
  *((unsigned long *)&__m128i_op0[0]) = 0xf359f359f359f359;
  *((unsigned long *)&__m128i_op1[1]) = 0xd3259a2984048c23;
  *((unsigned long *)&__m128i_op1[0]) = 0xf9796558e39953fd;
  *((unsigned long *)&__m128i_result[1]) = 0x86dd8341b164f12b;
  *((unsigned long *)&__m128i_result[0]) = 0x9611c3985b3159f5;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op0[0]) = 0xffd27db010d20fbf;
  *((unsigned long *)&__m128i_op1[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[0]) = 0xffd27db010d20fbf;
  *((unsigned long *)&__m128i_result[1]) = 0x9727b8499727b849;
  *((unsigned long *)&__m128i_result[0]) = 0x12755900b653f081;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_op0[0]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x02f3030303030303;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x06d9090909090909;
  __m128i_out = __lsx_vmul_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffff81ffff7f03;
  *((unsigned long *)&__m128i_op0[0]) = 0x04ffff8101ff81ff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0a0000001e000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0a000000f6000000;
  __m128i_out = __lsx_vmul_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x317fce80317fce80;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmul_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

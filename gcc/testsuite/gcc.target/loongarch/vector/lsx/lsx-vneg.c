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

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffeffffffff;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000004;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffefffefffffffc;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffff00ffffff01;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000fff3;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff0001ffffff0a;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100000101;
  *((unsigned long *)&__m128i_result[0]) = 0x000100ff010101f6;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff000000ff00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0xff00ff0000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0100000001000100;
  *((unsigned long *)&__m128i_result[0]) = 0x0100010000000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000100;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000101;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000fffffeff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffbff8888080a;
  *((unsigned long *)&__m128i_op0[0]) = 0x080803ff807ff7f9;
  *((unsigned long *)&__m128i_result[1]) = 0x010105017878f8f6;
  *((unsigned long *)&__m128i_result[0]) = 0xf8f8fd0180810907;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000080000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000300000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffdffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffeffff;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x441ba9fcffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x181b2541ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xbbe5560400010001;
  *((unsigned long *)&__m128i_result[0]) = 0xe7e5dabf00010001;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000060a3db;
  *((unsigned long *)&__m128i_op0[0]) = 0xa70594c000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ff9f5c25;
  *((unsigned long *)&__m128i_result[0]) = 0x58fa6b4000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vneg_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000010000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffff0000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000001f;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000008000001e;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffe1;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff7fffffe2;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x67eb85afb2ebb000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128i_result[1]) = 0x98147a504d145000;
  *((unsigned long *)&__m128i_result[0]) = 0x377b810912c0e000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffc00001ff800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x003ffffe00800000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100000000;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000001;
  __m128i_out = __lsx_vneg_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x087c000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000087c;
  *((unsigned long *)&__m128i_result[1]) = 0xf784000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffff784;
  __m128i_out = __lsx_vneg_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vneg_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

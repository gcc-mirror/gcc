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
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ca354688;
  *((unsigned long *)&__m128i_op1[1]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_op1[0]) = 0x0404038383838404;
  *((unsigned long *)&__m128i_result[1]) = 0x00040003ff83ff84;
  *((unsigned long *)&__m128i_result[0]) = 0x00040003ff4dffca;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000040d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000004;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00001f5400000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001f00000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000f80007;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0xb);
  *((unsigned long *)&__m128i_op0[1]) = 0x003fffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x003fffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff00000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffff0100ff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_result[0]) = 0xfffefffefffeffff;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x478b478b38031779;
  *((unsigned long *)&__m128i_op0[0]) = 0x6b769e690fa1e119;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000001030103;
  *((unsigned long *)&__m128i_result[1]) = 0x0047004700380017;
  *((unsigned long *)&__m128i_result[0]) = 0x006bff9e0010ffe2;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128i_op0[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128i_op1[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128i_op1[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128i_result[1]) = 0xff80ffa2fff0ff74;
  *((unsigned long *)&__m128i_result[0]) = 0xff76ffd8ffe6ffaa;
  __m128i_out = __lsx_vaddwod_h_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000800;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x1f54e0ab00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00001f5400000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000000f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op1[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_op1[0]) = 0xa352bfac9269e0aa;
  *((unsigned long *)&__m128i_result[1]) = 0xffffd70b00006ea9;
  *((unsigned long *)&__m128i_result[0]) = 0xffffa352ffff9269;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op1[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_op1[0]) = 0xa352bfac9269e0aa;
  *((unsigned long *)&__m128i_result[1]) = 0xffffd70b00006ea9;
  *((unsigned long *)&__m128i_result[0]) = 0xffffa352ffff9269;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0xe593c8c4e593c8c4;
  *((unsigned long *)&__m128i_op1[1]) = 0x8144ffff01c820a4;
  *((unsigned long *)&__m128i_op1[0]) = 0x9b2ee1a4034b4e34;
  *((unsigned long *)&__m128i_result[1]) = 0xffff80c400000148;
  *((unsigned long *)&__m128i_result[0]) = 0xffff80c1ffffe8de;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffefffffffe;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffefffffffe;
  __m128i_out = __lsx_vaddwod_w_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xa486c90f6537b8d7;
  *((unsigned long *)&__m128i_op0[0]) = 0x58bcc2013ea1cc1e;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffa486c90f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000058bcc201;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00001802041b0013;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x00001802041b0014;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000003004;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff02000200;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffe000ffffe000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffdfff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffdfff;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fbf83468;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fbf83468;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff82bb9784;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffc6bb97ac;
  *((unsigned long *)&__m128i_result[1]) = 0x000000007ffffffe;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001000fbff9;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000002ff9afef;
  *((unsigned long *)&__m128i_result[1]) = 0x000000004f804f81;
  *((unsigned long *)&__m128i_result[0]) = 0x000000004f804f80;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000001000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000020;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001000000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000fff0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000010;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffe00029f9f6061;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x64e464e464e464e4;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffeffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000064e264e6;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0305030203020502;
  *((unsigned long *)&__m128i_op0[0]) = 0x0301030203020502;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000003050302;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000003010302;
  __m128i_out = __lsx_vaddwod_d_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ff0000ff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x01fc020000fe0100;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff0000ff0000;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff84fff4ff84fff4;
  *((unsigned long *)&__m128i_op1[0]) = 0x00a6ffceffb60052;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xff84fff4ff84fff4;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000fefefe6a;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000c2bac2c2;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000fefefe6a;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0032000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffff0000;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5a57bacbd7e39680;
  *((unsigned long *)&__m128i_op0[0]) = 0x6bae051ffed76001;
  *((unsigned long *)&__m128i_op1[1]) = 0xf3e6586b60d7b152;
  *((unsigned long *)&__m128i_op1[0]) = 0xf7077b934ac0e000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x4e3e133738bb47d2;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000117d00007f7b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000093d0000187f;
  *((unsigned long *)&__m128i_op1[1]) = 0x7d7f027f7c7f7c79;
  *((unsigned long *)&__m128i_op1[0]) = 0x7e7f7e7f027f032f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7d7f13fc7c7ffbf4;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vaddwod_q_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

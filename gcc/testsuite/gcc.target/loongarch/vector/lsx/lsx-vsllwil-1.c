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

  *((unsigned long *)&__m128i_op0[1]) = 0x0020002000200020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0020002000200020;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000e0000000e0;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000fc00;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000fc00;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0xffeb48e03eab7ebe;
  *((unsigned long *)&__m128i_result[1]) = 0xffc0fac01200f800;
  *((unsigned long *)&__m128i_result[0]) = 0x0f80eac01f80ef80;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000e7e20468;
  *((unsigned long *)&__m128i_op0[0]) = 0xc2fac2fa53e7db29;
  *((unsigned long *)&__m128i_result[1]) = 0xff84fff4ff84fff4;
  *((unsigned long *)&__m128i_result[0]) = 0x00a6ffceffb60052;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x002e0059003b0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000005c000000b2;
  *((unsigned long *)&__m128i_result[0]) = 0x0000007600000000;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x2e34594c3b000000;
  *((unsigned long *)&__m128i_result[1]) = 0x017001a002c80260;
  *((unsigned long *)&__m128i_result[0]) = 0x01d8000000000000;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f4f4f4f4f4f4f4f;
  *((unsigned long *)&__m128i_result[1]) = 0x09e009e009e009e0;
  *((unsigned long *)&__m128i_result[0]) = 0x09e009e009e009e0;
  __m128i_out = __lsx_vsllwil_h_b (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000040000000400;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000005050000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0505000005050505;
  *((unsigned long *)&__m128i_result[1]) = 0x0028280000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0028280000282800;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffff800;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffc0000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffc0000000000000;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffff00ffffff00;
  *((unsigned long *)&__m128i_result[0]) = 0xffffff00ffffff00;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf10cf508f904fd01;
  *((unsigned long *)&__m128i_op0[0]) = 0xf10cf508f904fd01;
  *((unsigned long *)&__m128i_result[1]) = 0xffffe218ffffea10;
  *((unsigned long *)&__m128i_result[0]) = 0xfffff208fffffa02;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x21201f1e1d001b1a;
  *((unsigned long *)&__m128i_op0[0]) = 0x1918171615141312;
  *((unsigned long *)&__m128i_result[1]) = 0x0001918000017160;
  *((unsigned long *)&__m128i_result[0]) = 0x0001514000013120;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffff60ca7104649;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffff790a15db63d;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffc00ffde4000;
  *((unsigned long *)&__m128i_result[0]) = 0xfe857400fed8f400;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1c6c80007fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0038d800ff000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00fffe00fffffe00;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff800000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff800000000000;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsllwil_w_h (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001fffe00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000007fff800000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x007fffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff80ff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff80000000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000001fffe;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000104000800;
  *((unsigned long *)&__m128i_result[1]) = 0x0000040004000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0010002000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000017fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000800000;
  *((unsigned long *)&__m128i_result[0]) = 0x003fffffff800000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x57f160c4a1750eda;
  *((unsigned long *)&__m128i_result[1]) = 0x000002bf8b062000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffd0ba876d000;
  __m128i_out = __lsx_vsllwil_d_w (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

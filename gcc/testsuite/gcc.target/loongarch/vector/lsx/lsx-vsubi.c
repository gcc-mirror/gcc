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

  *((unsigned long *)&__m128i_op0[1]) = 0xfff489b693120950;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffc45a851c40c18;
  *((unsigned long *)&__m128i_result[1]) = 0xe0d56a9774f3ea31;
  *((unsigned long *)&__m128i_result[0]) = 0xe0dd268932a5edf9;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffff88;
  *((unsigned long *)&__m128i_result[1]) = 0xe5e5e5e5e5e5e5e5;
  *((unsigned long *)&__m128i_result[0]) = 0xe5e5e5e5e4e4e46d;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000897957687;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000408;
  *((unsigned long *)&__m128i_result[1]) = 0xf7f7f7ff8e8c6d7e;
  *((unsigned long *)&__m128i_result[0]) = 0xf7f7f7f7f7f7fbff;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x1);
  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xe6e6e6e6e6e6e6e6;
  *((unsigned long *)&__m128i_result[0]) = 0xe6e6e6e6e6e6e6e6;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m128i_result[0]) = 0xf8f8f8f8f8f8f8f8;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x2e34594c3b000000;
  *((unsigned long *)&__m128i_result[1]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m128i_result[0]) = 0x171d423524e9e9e9;
  __m128i_out = __lsx_vsubi_bu (__m128i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffe2ffe2ffe2ffe2;
  *((unsigned long *)&__m128i_result[0]) = 0xffe2ffe2ffe2ffe2;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x9795698585057dec;
  *((unsigned long *)&__m128i_op0[0]) = 0x87f82867431a1d08;
  *((unsigned long *)&__m128i_result[1]) = 0x9780697084f07dd7;
  *((unsigned long *)&__m128i_result[0]) = 0x87e3285243051cf3;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128i_result[0]) = 0xfffcfffcfffcfffc;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000101;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffcfffcfffc00fd;
  *((unsigned long *)&__m128i_result[0]) = 0xfffcfffcfffcfffc;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x371fe00000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x371fe00000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x370bdfecffecffec;
  *((unsigned long *)&__m128i_result[0]) = 0x370bdfecffecffec;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000040600000406;
  *((unsigned long *)&__m128i_op0[0]) = 0x020202020202fe02;
  *((unsigned long *)&__m128i_result[1]) = 0xfff503fbfff503fb;
  *((unsigned long *)&__m128i_result[0]) = 0x01f701f701f7fdf7;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffdfffdfffdfffd;
  *((unsigned long *)&__m128i_result[0]) = 0xfffdfffdfffdfffd;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x803e0000803e0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x803e0000803e0000;
  *((unsigned long *)&__m128i_result[1]) = 0x803bfffd803bfffd;
  *((unsigned long *)&__m128i_result[0]) = 0x803bfffd803bfffd;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffdfffdfffdfffd;
  *((unsigned long *)&__m128i_result[0]) = 0xfffdfffdfffdfffd;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffedffedffedffed;
  *((unsigned long *)&__m128i_result[0]) = 0xffedffedffedffed;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffe4ffe4ffe4ffe4;
  *((unsigned long *)&__m128i_result[0]) = 0xffe4ffe4ffe4ffe4;
  __m128i_out = __lsx_vsubi_hu (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffefffffffef;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffefffffffef;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffe6ffffffe6;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffe6ffffffe6;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff1fffffff1;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff1fffffff1;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff6fffffff6;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff6fffffff6;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffe4ffffffe4;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffe4ffffffe4;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffe1ffffffe1;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffe1ffffffe1;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffff1fffffff1;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffff1fffffff1;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffab7e71e33848;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffe1ffffffe1;
  *((unsigned long *)&__m128i_result[0]) = 0xffffab5f71e33829;
  __m128i_out = __lsx_vsubi_wu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xa8beed87bc3f2be1;
  *((unsigned long *)&__m128i_op0[0]) = 0x0024d8f6a494006a;
  *((unsigned long *)&__m128i_result[1]) = 0xa8beed87bc3f2bd3;
  *((unsigned long *)&__m128i_result[0]) = 0x0024d8f6a494005c;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffeb;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffe1;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffe1;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff7;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffe5;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffe5;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xf2f2e5e5e5e5e5e5;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xf2f2e5e5e5e5e5dc;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff7;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3fffff0000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3fffff0000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3ffffeffffffffe5;
  *((unsigned long *)&__m128i_result[0]) = 0x3ffffeffffffffe5;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000007b;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000070;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff5;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff0;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff0;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffe6;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffe6;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x000100010000fffb;
  *((unsigned long *)&__m128i_result[0]) = 0x000100010000fffb;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffeb;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffffa;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffdfffe80008000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffe2;
  *((unsigned long *)&__m128i_result[0]) = 0xfffdfffe80007fe2;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001a001a001a001a;
  *((unsigned long *)&__m128i_op0[0]) = 0x001a001a001a001a;
  *((unsigned long *)&__m128i_result[1]) = 0x001a001a001a000b;
  *((unsigned long *)&__m128i_result[0]) = 0x001a001a001a000b;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000234545b;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000c0dec4d1;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000002345454;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000c0dec4ca;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0f8d33000f8d3300;
  *((unsigned long *)&__m128i_op0[0]) = 0x0003b80000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0f8d33000f8d32fd;
  *((unsigned long *)&__m128i_result[0]) = 0x0003b7fffffffffd;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsubi_du (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

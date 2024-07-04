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
  *((unsigned long *)&__m128i_op0[0]) = 0x00001802041b0013;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00001802041b0013;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff0000ffff;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000ff000000ff;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0080000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xf4b6f3f52f4ef4a8;
  *((unsigned long *)&__m128i_result[1]) = 0xff80000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xf4b6f3f52f4ef4a8;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffff0000ffff;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f917d72d3d01e4;
  *((unsigned long *)&__m128i_op1[0]) = 0x203e16d116de012b;
  *((unsigned long *)&__m128i_result[1]) = 0x00f900d7003d00e4;
  *((unsigned long *)&__m128i_result[0]) = 0x003e00d100de002b;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010000000000;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc2f9bafac2fac2fa;
  *((unsigned long *)&__m128i_op1[1]) = 0xbdf077eee7e20468;
  *((unsigned long *)&__m128i_op1[0]) = 0xe3b1cc6953e7db29;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000e7e20468;
  *((unsigned long *)&__m128i_result[0]) = 0xc2fac2fa53e7db29;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xf8f8e018f8f8e810;
  *((unsigned long *)&__m128i_op1[0]) = 0xf8f8f008f8f8f800;
  *((unsigned long *)&__m128i_result[1]) = 0x0000e0180000e810;
  *((unsigned long *)&__m128i_result[0]) = 0x0000f0080000f800;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1211100f11100f0e;
  *((unsigned long *)&__m128i_op0[0]) = 0x100f0e0d0f0e0d0c;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[1]) = 0x11000f2010000e20;
  *((unsigned long *)&__m128i_result[0]) = 0x0f000d200e000c20;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xe3e3e3e3e3e3e3e3;
  *((unsigned long *)&__m128i_op1[0]) = 0xe3e3e3e3e3e3e3e3;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xe3e3e3e3e3e3e3e3;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7ffe7ffe7ffe7ffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00007ffe00007ffe;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001c00ffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00007f7f00007f7f;
  *((unsigned long *)&__m128i_result[1]) = 0x000001000f00fe00;
  *((unsigned long *)&__m128i_result[0]) = 0x0000017fff00fe7f;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000f0009d3c;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000016fff9d3d;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffff000f0008d3c;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffff0016fff8d3d;
  *((unsigned long *)&__m128i_result[1]) = 0x00ff000000003c3c;
  *((unsigned long *)&__m128i_result[0]) = 0x00ff0101ffff3d3d;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000958affff995d;
  *((unsigned long *)&__m128i_result[1]) = 0xffff000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000958affff995d;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100013fa0;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffefffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffefffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffefefffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0002fffefffd0001;
  *((unsigned long *)&__m128i_op1[1]) = 0x1202120212021202;
  *((unsigned long *)&__m128i_op1[0]) = 0x1202120212021202;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_result[0]) = 0x0202fe02fd020102;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5a6f5c53ebed3faa;
  *((unsigned long *)&__m128i_op0[0]) = 0xa36aca4435b8b8e1;
  *((unsigned long *)&__m128i_op1[1]) = 0x5a6f5c53ebed3faa;
  *((unsigned long *)&__m128i_op1[0]) = 0xa36aca4435b8b8e1;
  *((unsigned long *)&__m128i_result[1]) = 0x5c535c533faa3faa;
  *((unsigned long *)&__m128i_result[0]) = 0xca44ca44b8e1b8e1;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_op0[0]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_op1[1]) = 0x77c0404a4000403a;
  *((unsigned long *)&__m128i_op1[0]) = 0x77c03fd640003fc6;
  *((unsigned long *)&__m128i_result[1]) = 0x04c0044a0400043a;
  *((unsigned long *)&__m128i_result[0]) = 0x04c004d6040004c6;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000006362ffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000d0000000d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000dffff000d;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000010000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000002002;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_result[0]) = 0x0002000200020002;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_result[1]) = 0x2080208020802080;
  *((unsigned long *)&__m128i_result[0]) = 0x2080208020802080;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000001b0000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000001b0000;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000053a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff9000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc000400000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffc000400000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001f00000000;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0xe593c8c4e593c8c4;
  *((unsigned long *)&__m128i_op1[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_result[1]) = 0x8080000080800000;
  *((unsigned long *)&__m128i_result[0]) = 0x9380c4009380c400;
  __m128i_out = __lsx_vpackev_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffc2007aff230027;
  *((unsigned long *)&__m128i_op0[0]) = 0x0080005eff600001;
  *((unsigned long *)&__m128i_op1[1]) = 0x01017f3c00000148;
  *((unsigned long *)&__m128i_op1[0]) = 0x117d7f7b093d187f;
  *((unsigned long *)&__m128i_result[1]) = 0xff23002700000148;
  *((unsigned long *)&__m128i_result[0]) = 0xff600001093d187f;
  __m128i_out = __lsx_vpackev_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff0000ffff0000;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0002711250a27112;
  *((unsigned long *)&__m128i_op1[0]) = 0x00d2701294027112;
  *((unsigned long *)&__m128i_result[1]) = 0xffff7112ffff7112;
  *((unsigned long *)&__m128i_result[0]) = 0xffff7012ffff7112;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x30eb020302101b03;
  *((unsigned long *)&__m128i_op0[0]) = 0x020310d0c0030220;
  *((unsigned long *)&__m128i_op1[1]) = 0x30eb020302101b03;
  *((unsigned long *)&__m128i_op1[0]) = 0x020310d0c0030220;
  *((unsigned long *)&__m128i_result[1]) = 0x020310d0c0030220;
  *((unsigned long *)&__m128i_result[0]) = 0x020310d0c0030220;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000eefff;
  *((unsigned long *)&__m128i_op0[0]) = 0xf8e1a03affffe3e2;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000000eefff;
  *((unsigned long *)&__m128i_op1[0]) = 0xf8e1a03affffe3e2;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000efffefff;
  *((unsigned long *)&__m128i_result[0]) = 0xa03aa03ae3e2e3e2;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_op1[0]) = 0x27b169bbb8140001;
  *((unsigned long *)&__m128i_result[1]) = 0x000010f8000081a2;
  *((unsigned long *)&__m128i_result[0]) = 0x000069bb00000001;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vpackev_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

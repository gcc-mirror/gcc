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

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x3f3f3f3f3f3f3f3f;
  *((unsigned long *)&__m256i_result[2]) = 0x3f3f3f3f3f3f3f3f;
  *((unsigned long *)&__m256i_result[1]) = 0x000000003f3f3f3f;
  *((unsigned long *)&__m256i_result[0]) = 0x3f3f3f3f00000000;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000ffff;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000001ffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xfe00000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000001ffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xfe00000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000017f7f7f;
  *((unsigned long *)&__m256i_result[2]) = 0x7f00000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000017f7f7f;
  *((unsigned long *)&__m256i_result[0]) = 0x7f00000000000000;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000700000007;
  *((unsigned long *)&__m256i_op0[2]) = 0x0007ffff0007ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000700000007;
  *((unsigned long *)&__m256i_op0[0]) = 0x0007ffff0007ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000700000007;
  *((unsigned long *)&__m256i_result[2]) = 0x00071f1f00071f1f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000700000007;
  *((unsigned long *)&__m256i_result[0]) = 0x00071f1f00071f1f;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000d6d6d;
  __m256i_out = __lasx_xvsat_bu (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[2]) = 0x000000003fff3fff;
  *((unsigned long *)&__m256i_result[1]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[0]) = 0x000000003fff3fff;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_op0[2]) = 0x2020202020206431;
  *((unsigned long *)&__m256i_op0[1]) = 0x2020202020202020;
  *((unsigned long *)&__m256i_op0[0]) = 0x2020202020206431;
  *((unsigned long *)&__m256i_result[3]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_result[2]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_result[1]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_result[0]) = 0x001f001f001f001f;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[2]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[1]) = 0x00001fff00001fff;
  *((unsigned long *)&__m256i_result[0]) = 0x00001fff00001fff;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000007fff7fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000007fff7fff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000003f003f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000003f003f;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xc0090000c0200060;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xc0090000c0200060;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x007f0000007f0060;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x007f0000007f0060;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x3eab77367fff4848;
  *((unsigned long *)&__m256i_op0[2]) = 0x408480007fff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x3eab77367fff4848;
  *((unsigned long *)&__m256i_op0[0]) = 0x408480007fff0000;
  *((unsigned long *)&__m256i_result[3]) = 0x0003000300030003;
  *((unsigned long *)&__m256i_result[2]) = 0x0003000300030000;
  *((unsigned long *)&__m256i_result[1]) = 0x0003000300030003;
  *((unsigned long *)&__m256i_result[0]) = 0x0003000300030000;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000001fff000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000029170;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000001fff000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000029170;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000001ff03ff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000203ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000001ff03ff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000203ff;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_hu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x457db03e457db03e;
  *((unsigned long *)&__m256i_op0[2]) = 0x457db03e45a87310;
  *((unsigned long *)&__m256i_op0[1]) = 0x457db03e457db03e;
  *((unsigned long *)&__m256i_op0[0]) = 0x457db03e45a87310;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000f0000000f;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000077fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000003ffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00003fe000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00003fe000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00003fe000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00003fe000000000;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000fffcfffc;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000fffcfffc;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000fffcfffc;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000fffcfffc;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000003fff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000003fff;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7efefefe80ffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0fffffff0fffffff;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffe000ffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffe000ffffffff08;
  *((unsigned long *)&__m256i_op0[1]) = 0xffe000ffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffe000ffffffff08;
  *((unsigned long *)&__m256i_result[3]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0fffffff0fffffff;
  __m256i_out = __lasx_xvsat_wu (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x000003ffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x000003ffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x000003ffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x000003ffffffffff;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x29);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x34);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff00ff00000007;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000e7;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff00ff00000007;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[3]) = 0x0001ffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000000e7;
  *((unsigned long *)&__m256i_result[1]) = 0x0001ffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000007;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x30);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x247fe49409620040;
  *((unsigned long *)&__m256i_op0[2]) = 0x2475cef801f0ffdd;
  *((unsigned long *)&__m256i_op0[1]) = 0x6580668200fe0002;
  *((unsigned long *)&__m256i_op0[0]) = 0x419cd5b11c3c5654;
  *((unsigned long *)&__m256i_result[3]) = 0x247fe49409620040;
  *((unsigned long *)&__m256i_result[2]) = 0x2475cef801f0ffdd;
  *((unsigned long *)&__m256i_result[1]) = 0x6580668200fe0002;
  *((unsigned long *)&__m256i_result[0]) = 0x419cd5b11c3c5654;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x3f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x22);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff800080000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff800000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000000001ff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000001ff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000000001ff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000001ff;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000007fffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000007fffff;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000017f00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00007f7f03030000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000017f00000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00007f7f03030000;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x37);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_du (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x7);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0x4);
  int_result = 0x0000000000000000;
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ff0000ff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x01fc020000fe0100;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x7);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000463fd2902d;
  *((unsigned long *)&__m128i_op0[0]) = 0x5ccd54bbfcac806c;
  unsigned_int_result = 0x00000000000000ac;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x697eba2bedfa9c82;
  *((unsigned long *)&__m128i_op0[0]) = 0xd705c77a7025c899;
  unsigned_int_result = 0x000000000000edfa;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x5);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0400040004000400;
  *((unsigned long *)&__m128i_op0[0]) = 0x0400040004000400;
  unsigned_int_result = 0x0000000000000400;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x5);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007d3ac600;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0x7);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1dffbfff00000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0200400000000001;
  unsigned_int_result = 0x0000000000000001;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000003fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000003fffffff;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000490000004d;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001ffffffff;
  long_int_result = 0x00000001ffffffff;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff84fff4ff84fff4;
  *((unsigned long *)&__m128i_op0[0]) = 0x00a6ffceffb60052;
  unsigned_int_result = 0x0000000000000084;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0xa);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101010101010101;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0xc);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  unsigned_int_result = 0x00000000ffffffff;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  long_int_result = 0xffffffffffffffff;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3f8000003f800000;
  unsigned_long_int_result = 0x3f8000003f800000;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0bd80bd80bd80bd8;
  *((unsigned long *)&__m128i_op0[0]) = 0x0bd80bd80bd80bd8;
  unsigned_long_int_result = 0x0bd80bd80bd80bd8;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0x8);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000c0000bd49;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000c7fff000c;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0xb);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000000000ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000100c6ffef10c;
  unsigned_int_result = 0x00000000000000ff;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128i_op0[0]) = 0x2020202020207f7f;
  unsigned_int_result = 0x0000000020202020;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000003a24;
  *((unsigned long *)&__m128i_op0[0]) = 0x003dbe88077c78c1;
  int_result = 0x0000000000003a24;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  unsigned_int_result = 0x00000000000000ff;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0x9);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0002000200000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0xb);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffe080f6efc100f7;
  *((unsigned long *)&__m128i_op0[0]) = 0xefd32176ffe100f7;
  int_result = 0x0000000000002176;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000200000002;
  int_result = 0x0000000000000002;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff00ff00ff00ff;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0x5);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffc0ff80ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x80008000ec82ab51;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000800089e08000;
  int_result = 0x0000000089e08000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6453f5e01d6e5000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fdec000000000;
  int_result = 0x000000001d6e5000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6453f5e01d6e5000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fdec000000000;
  int_result = 0x0000000001d6e5000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00009c7c00007176;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0xe);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8d78336c83652b86;
  *((unsigned long *)&__m128i_op0[0]) = 0x39c51f389c0d6112;
  int_result = 0x000000009c0d6112;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff0000857a;
  *((unsigned long *)&__m128i_op0[0]) = 0x05fafe0101fe000e;
  unsigned_int_result = 0x000000000000857a;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x021b7d2449678a35;
  *((unsigned long *)&__m128i_op0[0]) = 0x030298a621030a49;
  int_result = 0x00000000ffff8a35;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op0[0]) = 0x001effae001effae;
  unsigned_int_result = 0x000000000000001e;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00009c7c00007176;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0xe);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8d78336c83652b86;
  *((unsigned long *)&__m128i_op0[0]) = 0x39c51f389c0d6112;
  int_result = 0x000000009c0d6112;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff0000857a;
  *((unsigned long *)&__m128i_op0[0]) = 0x05fafe0101fe000e;
  unsigned_int_result = 0x000000000000857a;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x021b7d2449678a35;
  *((unsigned long *)&__m128i_op0[0]) = 0x030298a621030a49;
  int_result = 0x00000000ffff8a35;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op0[0]) = 0x001effae001effae;
  unsigned_int_result = 0x000000000000001e;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0x8);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x000000000000001e;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x5);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000003ddc5dac;
  long_int_result = 0x000000003ddc5dac;
  long_int_out = __lsx_vpickve2gr_d (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6453f5e01d6e5000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fdec000000000;
  int_result = 0x000000001d6e5000;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00009c7c00007176;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_b (__m128i_op0, 0xe);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8d78336c83652b86;
  *((unsigned long *)&__m128i_op0[0]) = 0x39c51f389c0d6112;
  int_result = 0x000000009c0d6112;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff0000857a;
  *((unsigned long *)&__m128i_op0[0]) = 0x05fafe0101fe000e;
  unsigned_int_result = 0x000000000000857a;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_out, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x021b7d2449678a35;
  *((unsigned long *)&__m128i_op0[0]) = 0x030298a621030a49;
  int_result = 0x00000000ffff8a35;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x4);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001effae001effae;
  *((unsigned long *)&__m128i_op0[0]) = 0x001effae001effae;
  unsigned_int_result = 0x000000000000001e;
  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0674c8868a74fc80;
  *((unsigned long *)&__m128i_op0[0]) = 0xfdce8003090b0906;
  int_result = 0x00000000090b0906;
  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x3);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000feff23560000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000fd1654860000;
  unsigned_int_result = 0x00000000000000ff;
  unsigned_int_out = __lsx_vpickve2gr_bu (__m128i_op0, 0xc);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4f4f00004f4f0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f4f00004f4f0000;
  unsigned_int_result = 0x000000004f4f0000;
  unsigned_int_out = __lsx_vpickve2gr_wu (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000120000000d;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000e0000000e;
  unsigned_long_int_result = 0x0000000e0000000e;
  unsigned_long_int_out = __lsx_vpickve2gr_du (__m128i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  return 0;
}

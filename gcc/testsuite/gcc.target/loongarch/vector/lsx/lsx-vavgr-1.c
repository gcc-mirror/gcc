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
  *((unsigned long *)&__m128i_op0[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0040000000ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0040000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0020000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0020c00000000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xb9fe3640e4eb1b18;
  *((unsigned long *)&__m128i_op0[0]) = 0x800000005b4b1b18;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffd000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xdcfe1b20f2f60e0c;
  *((unsigned long *)&__m128i_result[0]) = 0xc00000002e260e0c;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x111110ff11111141;
  *((unsigned long *)&__m128i_op0[0]) = 0x1111113111111121;
  *((unsigned long *)&__m128i_op1[1]) = 0xfbffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x7bffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x060808ff08080820;
  *((unsigned long *)&__m128i_result[0]) = 0x4608081808080810;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000010;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000000fff0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000ac26;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff80000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000060000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000003000000d613;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000c0000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffe5;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffe5;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffff2;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffff2;
  __m128i_out = __lsx_vavgr_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000073;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000002a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000003a;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000100000015;
  __m128i_out = __lsx_vavgr_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000004000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfff8004000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffc002000000000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffc002000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc002000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffc002000000000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000ff00000000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000007fff0018;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000003fff800c;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0280000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7500000075000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7500000075000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3a8000003a800000;
  *((unsigned long *)&__m128i_result[0]) = 0x3bc000003a800000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff800000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00007d1800007c99;
  *((unsigned long *)&__m128i_op1[1]) = 0x0a0000001e000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0a621b3ebe5e1c02;
  *((unsigned long *)&__m128i_result[1]) = 0x04ffc0000f000000;
  *((unsigned long *)&__m128i_result[0]) = 0x05314c2bdf2f4c4e;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001000000000;
  __m128i_out = __lsx_vavgr_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff80000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_op1[0]) = 0x7f8000007f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x3fc000003fc00000;
  *((unsigned long *)&__m128i_result[0]) = 0x3fc000003fc00000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x3fffffffc0000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op1[0]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_result[1]) = 0xff807f807f807f80;
  *((unsigned long *)&__m128i_result[0]) = 0xff807f807f807f80;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000002;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000280000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000140001;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffff46;
  *((unsigned long *)&__m128i_op1[1]) = 0x00fe00fe00fe00fe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00fe00fe00fe0045;
  *((unsigned long *)&__m128i_result[1]) = 0x007f007f007f007e;
  *((unsigned long *)&__m128i_result[0]) = 0x007f007f007effc6;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x67eb85afb2ebb000;
  *((unsigned long *)&__m128i_op0[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m128i_result[1]) = 0x33f5c2d7d975d7fe;
  *((unsigned long *)&__m128i_result[0]) = 0xe4423f7b769f8ffe;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000003ff8;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xff9dff9dff9dff9d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffceffceffcf1fcb;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3a8000003a800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3bc000003a800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00fe00fe7fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x1d4000001d400000;
  *((unsigned long *)&__m128i_result[0]) = 0x1e5f007f5d400000;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ff00;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000400000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000007f80;
  __m128i_out = __lsx_vavgr_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

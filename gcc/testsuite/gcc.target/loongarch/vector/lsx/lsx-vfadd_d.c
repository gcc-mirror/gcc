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

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000fea8ff44;
  *((unsigned long *)&__m128d_op1[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128d_op1[0]) = 0x2020202020202020;
  *((unsigned long *)&__m128d_result[1]) = 0x2020202020202020;
  *((unsigned long *)&__m128d_result[0]) = 0x2020202020202020;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128d_op0[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128d_result[0]) = 0x1000100010001000;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000000000000f;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x000000000000000f;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000010100fe0101;
  *((unsigned long *)&__m128d_op0[0]) = 0xffff0200ffff01ff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0001010100fe0100;
  *((unsigned long *)&__m128d_result[0]) = 0xffff0200ffff01ff;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7fff0101ffffe000;
  *((unsigned long *)&__m128d_op0[0]) = 0x7fffffffa0204000;
  *((unsigned long *)&__m128d_op1[1]) = 0x7f370101ff04ffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x7f3bffffa0226021;
  *((unsigned long *)&__m128d_result[1]) = 0x7fff0101ffffe000;
  *((unsigned long *)&__m128d_result[0]) = 0x7fffffffa0204000;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000ebd20000714f;
  *((unsigned long *)&__m128d_op0[0]) = 0x00012c8a0000a58a;
  *((unsigned long *)&__m128d_op1[1]) = 0xf654ad7447e59090;
  *((unsigned long *)&__m128d_op1[0]) = 0x27b1b106b8145f50;
  *((unsigned long *)&__m128d_result[1]) = 0xf654ad7447e59090;
  *((unsigned long *)&__m128d_result[0]) = 0x27b1b106b8145f50;
  __m128d_out = __lsx_vfadd_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000001300000013;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000100000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x1000100000001000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000100000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x1000100000001000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128d_op0[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000007000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x01533b5e7489ae24;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffab7e71e33848;
  *((unsigned long *)&__m128d_op1[1]) = 0x01533b5e7489ae24;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffab7e71e33848;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffab7e71e33848;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfmul_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128d_result[1]) = 0x800000ff000000ff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x00000000fff8fff8;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000fff80000;
  *((unsigned long *)&__m128d_result[1]) = 0x80000000fff8fff8;
  *((unsigned long *)&__m128d_result[0]) = 0x80000000fff80000;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128d_op1[1]) = 0xb55ccf30f52a6a68;
  *((unsigned long *)&__m128d_op1[0]) = 0x4e0018eceb82c53a;
  *((unsigned long *)&__m128d_result[1]) = 0x355ccf30f52a6a68;
  *((unsigned long *)&__m128d_result[0]) = 0xce0018eceb82c53a;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffff00006c82;
  *((unsigned long *)&__m128d_op0[0]) = 0x00009b140000917b;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffff00006c82;
  *((unsigned long *)&__m128d_result[0]) = 0x00009b140000917b;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000100000020;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000083b00000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xe93d0bd19ff0c170;
  *((unsigned long *)&__m128d_op0[0]) = 0x5237c1bac9eadf55;
  *((unsigned long *)&__m128d_op1[1]) = 0xe6d4572c8a5835bc;
  *((unsigned long *)&__m128d_op1[0]) = 0xe5017c2ac9ca9fd0;
  *((unsigned long *)&__m128d_result[1]) = 0xe93d0bd19ff07013;
  *((unsigned long *)&__m128d_result[0]) = 0x65017c2ac9ca9fd0;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xe93d0bd19ff07013;
  *((unsigned long *)&__m128d_op0[0]) = 0x65017c2ac9ca9fd0;
  *((unsigned long *)&__m128d_op1[1]) = 0x00008bf700017052;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000f841000091aa;
  *((unsigned long *)&__m128d_result[1]) = 0xe93d0bd19ff07013;
  *((unsigned long *)&__m128d_result[0]) = 0x65017c2ac9ca9fd0;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000004000000002;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x5555410154551515;
  *((unsigned long *)&__m128d_op1[0]) = 0x0004455501500540;
  *((unsigned long *)&__m128d_result[1]) = 0xd555410154551515;
  *((unsigned long *)&__m128d_result[0]) = 0x8004455501500540;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x000300037ff000ff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0003000300a10003;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000007ff000ff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0003000300000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0003000300a10003;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x56a09e662ab46b31;
  *((unsigned long *)&__m128d_op1[0]) = 0xb4b8122ef4054bb3;
  *((unsigned long *)&__m128d_result[1]) = 0xd6a09e662ab46b31;
  *((unsigned long *)&__m128d_result[0]) = 0x34b8122ef4054bb3;
  __m128d_out = __lsx_vfsub_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7f4000007f040000;
  *((unsigned long *)&__m128d_op0[0]) = 0x7f0200007f020000;
  *((unsigned long *)&__m128d_op1[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffff01018888;
  *((unsigned long *)&__m128d_result[1]) = 0xfffffffff8f8dada;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffff01018888;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x8000000080000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000100007f01;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffefefffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128d_op1[0]) = 0x0400000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffefefffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff8000000000000;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000000ff801c9e;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000810000;
  *((unsigned long *)&__m128d_op1[1]) = 0x000000000000ffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x40eff02383e383e4;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0xc8847ef6ed3f2000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0001000000010000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000cd630000cd63;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0xffff00000000ffff;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffff00000000ffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x000aa822a79308f6;
  *((unsigned long *)&__m128d_op1[0]) = 0x03aa558e1d37b5a1;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfffffffefffffffe;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffffffefffffffe;
  *((unsigned long *)&__m128d_op1[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128d_op1[0]) = 0xfffefffe011df03e;
  *((unsigned long *)&__m128d_result[1]) = 0xfffffffefffffffe;
  *((unsigned long *)&__m128d_result[0]) = 0xfffffffefffffffe;
  __m128d_out = __lsx_vfdiv_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}

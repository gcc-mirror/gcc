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

  *((int *)&__m128_op0[3]) = 0x56411278;
  *((int *)&__m128_op0[2]) = 0x43c0d41e;
  *((int *)&__m128_op0[1]) = 0x0124d8f6;
  *((int *)&__m128_op0[0]) = 0xa494006b;
  *((int *)&__m128_op1[3]) = 0x7f800000;
  *((int *)&__m128_op1[2]) = 0xff800000;
  *((int *)&__m128_op1[1]) = 0xff800000;
  *((int *)&__m128_op1[0]) = 0xff800000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x84939413;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000002;
  *((int *)&__m128_op0[0]) = 0xbefcb21e;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xfffefff6;
  *((int *)&__m128_op0[0]) = 0xfff80002;
  *((int *)&__m128_op1[3]) = 0x000000c5;
  *((int *)&__m128_op1[2]) = 0xac01015b;
  *((int *)&__m128_op1[1]) = 0xaaacac88;
  *((int *)&__m128_op1[0]) = 0xa3a9a96a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xff84fff4;
  *((int *)&__m128_op0[2]) = 0xff84fff4;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xfffffff0;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x11000f20;
  *((int *)&__m128_op0[2]) = 0x10000e20;
  *((int *)&__m128_op0[1]) = 0x0f000d20;
  *((int *)&__m128_op0[0]) = 0x0e000c20;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000c00;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00bd003d;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000005;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000005;
  *((int *)&__m128_op1[3]) = 0xfffefffe;
  *((int *)&__m128_op1[2]) = 0xfffefffe;
  *((int *)&__m128_op1[1]) = 0xfffefffe;
  *((int *)&__m128_op1[0]) = 0xfffefffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xff800001;
  *((int *)&__m128_op0[0]) = 0x0f800000;
  *((int *)&__m128_op1[3]) = 0x00000009;
  *((int *)&__m128_op1[2]) = 0x00000009;
  *((int *)&__m128_op1[1]) = 0xff80000a;
  *((int *)&__m128_op1[0]) = 0x0f800009;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x0000ffff;
  *((int *)&__m128_op0[1]) = 0x3b5eae24;
  *((int *)&__m128_op0[0]) = 0xab7e3848;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00003f80;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x63636363;
  *((int *)&__m128_op0[2]) = 0x3f3e47c1;
  *((int *)&__m128_op0[1]) = 0x41f8e080;
  *((int *)&__m128_op0[0]) = 0xf1ef4eaa;
  *((int *)&__m128_op1[3]) = 0x0000cecd;
  *((int *)&__m128_op1[2]) = 0x00004657;
  *((int *)&__m128_op1[1]) = 0x0000c900;
  *((int *)&__m128_op1[0]) = 0x00011197;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_clt_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x454c2996;
  *((int *)&__m128_op0[2]) = 0x0ffe354e;
  *((int *)&__m128_op0[1]) = 0x9e063f80;
  *((int *)&__m128_op0[0]) = 0x2742ba3e;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x42652524;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00070000;
  *((int *)&__m128_op0[2]) = 0x00050000;
  *((int *)&__m128_op0[1]) = 0x00030000;
  *((int *)&__m128_op0[0]) = 0x00010000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0xff81007c;
  *((int *)&__m128_op1[1]) = 0xffb7005f;
  *((int *)&__m128_op1[0]) = 0x0070007c;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x0000006f;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xfbffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x7bffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0xfbffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0x7bffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x0002a000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x0002a000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xfc606ec5;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x14155445;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x01030103;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00060fbf;
  *((int *)&__m128_op0[2]) = 0x02040fbf;
  *((int *)&__m128_op0[1]) = 0x00020fbf;
  *((int *)&__m128_op0[0]) = 0x02000fbf;
  *((int *)&__m128_op1[3]) = 0x63636363;
  *((int *)&__m128_op1[2]) = 0x63636363;
  *((int *)&__m128_op1[1]) = 0xffd27db0;
  *((int *)&__m128_op1[0]) = 0x10d20fbf;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x7f800000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000008;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000010000003f;
  *((unsigned long *)&__m128d_op1[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000010000003f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000003;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000036de0000;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000003be14000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x1111113111111141;
  *((unsigned long *)&__m128d_op0[0]) = 0x1111113111111121;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0032000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000080000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xe93d0bd19ff07013;
  *((unsigned long *)&__m128d_op1[0]) = 0x65017c2ac9ca9fd0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x007f007f007f007e;
  *((unsigned long *)&__m128d_op1[0]) = 0x007f007f007effc6;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000015800000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000158;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128d_op0[0]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128d_op1[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128d_op1[0]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7f8100017f810001;
  *((unsigned long *)&__m128d_op0[0]) = 0x7f8100017f810001;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_clt_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x004200a000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x004200a000200001;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x7fff00007fff7fff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128d_op1[0]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000000000001e;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000455555555;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000001580000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000001f0000001f;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_cult_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

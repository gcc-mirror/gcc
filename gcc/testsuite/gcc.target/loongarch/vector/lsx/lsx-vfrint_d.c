/* { dg-options "-mlsx -w -fno-strict-aliasing" } */
/* { dg-timeout 500 } */
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

  *((unsigned long *)&__m128d_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0003000300030003;
  *((unsigned long *)&__m128d_op0[0]) = 0x0003000700020005;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x7ff0000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00ff000100ff00fe;
  *((unsigned long *)&__m128d_op0[0]) = 0x00ff003000ff00a0;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfd200ed2fd370775;
  *((unsigned long *)&__m128d_op0[0]) = 0x96198318780e32c5;
  *((unsigned long *)&__m128d_result[1]) = 0xfd200ed2fd370775;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfrint_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrne_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xe0404041e0404041;
  *((unsigned long *)&__m128d_op0[0]) = 0xe0404041e0404041;
  *((unsigned long *)&__m128d_result[1]) = 0xe0404041e0404041;
  *((unsigned long *)&__m128d_result[0]) = 0xe0404041e0404041;
  __m128d_out = __lsx_vfrintrne_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000080800000808;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000080800000808;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrne_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfrintrne_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000868686868686;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrne_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000002000;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffc002000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xfffc002000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x9c9c9c9c00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00007fff00007fff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000007f00ff00ff;
  *((unsigned long *)&__m128d_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128d_result[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x3ff0000000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000077af9450;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x3ff0000000000000;
  __m128d_out = __lsx_vfrintrp_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff02ff1bff02ff23;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000ffffff02fff4;
  *((unsigned long *)&__m128d_result[1]) = 0xff02ff1bff02ff23;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op0[0]) = 0x6a57a30ff0000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x6a57a30ff0000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000001300000013;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffff02000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x1f81e3779b97f4a8;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffff02000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0001000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0001000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrm_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x98ff98ff220e220d;
  *((unsigned long *)&__m128d_op0[0]) = 0xa2e1a2601ff01ff0;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00000000abba7980;
  *((unsigned long *)&__m128d_op0[0]) = 0x00000000ccf98000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfe3bfb01fe3bfe01;
  *((unsigned long *)&__m128d_op0[0]) = 0xfe03fe3ffe01fa21;
  *((unsigned long *)&__m128d_result[1]) = 0xfe3bfb01fe3bfe01;
  *((unsigned long *)&__m128d_result[0]) = 0xfe03fe3ffe01fa21;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x5847b72626ce61ef;
  *((unsigned long *)&__m128d_op0[0]) = 0x110053f401e7cced;
  *((unsigned long *)&__m128d_result[1]) = 0x5847b72626ce61ef;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfrintrz_d (__m128d_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}

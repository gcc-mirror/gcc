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

  *((unsigned long *)&__m128i_result[1]) = 0x00a300a300a300a3;
  *((unsigned long *)&__m128i_result[0]) = 0x00a300a300a300a3;
  __m128i_out = __lsx_vldi (1187);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0xfffffffffffffe15;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffffffffe15;
  __m128i_out = __lsx_vldi (3605);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0xecececececececec;
  *((unsigned long *)&__m128i_result[0]) = 0xecececececececec;
  __m128i_out = __lsx_vldi (1004);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0x00ffff00ff00ff00;
  *((unsigned long *)&__m128i_result[0]) = 0x00ffff00ff00ff00;
  __m128i_out = __lsx_vldi (-1686);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  unsigned_int_out = __lsx_vpickve2gr_hu (__m128i_op0, 0x3);
  *((unsigned long *)&__m128i_result[1]) = 0x004d004d004d004d;
  *((unsigned long *)&__m128i_result[0]) = 0x004d004d004d004d;
  __m128i_out = __lsx_vldi (1101);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0000000a000000;
  __m128i_out = __lsx_vldi (-3318);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0x00ffff00ff00ff00;
  *((unsigned long *)&__m128i_result[0]) = 0x00ffff00ff00ff00;
  __m128i_out = __lsx_vldi (-1686);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_result[1]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0a0000000a000000;
  __m128i_out = __lsx_vldi (-3318);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

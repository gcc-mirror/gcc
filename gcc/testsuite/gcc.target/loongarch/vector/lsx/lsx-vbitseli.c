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

  *((unsigned long *)&__m128i_op0[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x6664666466646664;
  *((unsigned long *)&__m128i_result[0]) = 0x6664666466646664;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0x66);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffff7;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffff00000001;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffff0000010000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000001;
  *((unsigned long *)&__m128i_result[0]) = 0x5d5d5d5d5d5d5d55;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0x5d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x2);
  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_result[1]) = 0x5959595959595959;
  *((unsigned long *)&__m128i_result[0]) = 0x5959595959595959;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0x59);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffd000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xfffd000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0x3a);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0xaa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0b4c600000000002;
  *((unsigned long *)&__m128i_op1[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_op1[0]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0004280808080808;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0xa4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000004000000040;
  *((unsigned long *)&__m128i_op0[0]) = 0x00007770ffff9411;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_op1[0]) = 0x00007770ffff941d;
  *((unsigned long *)&__m128i_result[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_result[0]) = 0x000047404f4f040d;
  __m128i_out = __lsx_vbitseli_b (__m128i_op0, __m128i_op1, 0x4f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

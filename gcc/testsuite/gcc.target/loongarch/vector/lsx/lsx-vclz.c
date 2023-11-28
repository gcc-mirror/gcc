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

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000ff000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010000800100008;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000001fc1a568;
  *((unsigned long *)&__m128i_op0[0]) = 0x02693fe0e7beb077;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000030000;
  *((unsigned long *)&__m128i_result[0]) = 0x0006000200000000;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7f7f000b000b000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000b000b010a000b;
  *((unsigned long *)&__m128i_result[1]) = 0x0101080408040804;
  *((unsigned long *)&__m128i_result[0]) = 0x0804080407040804;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1ffffffff8001000;
  *((unsigned long *)&__m128i_op0[0]) = 0xf0bd80bd80bd8000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000003;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000100010000fe7c;
  *((unsigned long *)&__m128i_op0[0]) = 0x000100010000fe01;
  *((unsigned long *)&__m128i_result[1]) = 0x000f000f00100000;
  *((unsigned long *)&__m128i_result[0]) = 0x000f000f00100000;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x41dfffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0100000008080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000040;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000039;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000039;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff000100ff00fe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff003000ff00a0;
  *((unsigned long *)&__m128i_result[1]) = 0x0008000f00080008;
  *((unsigned long *)&__m128i_result[0]) = 0x0008000a00080008;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfe813f00fe813f00;
  *((unsigned long *)&__m128i_op0[0]) = 0xfe813f00fe813f00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000200000002;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000bffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000040;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000c0c00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x687a8373f249bc44;
  *((unsigned long *)&__m128i_op0[0]) = 0x7861145d9241a14a;
  *((unsigned long *)&__m128i_result[1]) = 0x0101000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0101030100010001;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000040;
  __m128i_out = __lsx_vclz_d (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000020;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080700000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000f0000000f;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000001f;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000008000001e;
  *((unsigned long *)&__m128i_result[1]) = 0x000000200000001b;
  *((unsigned long *)&__m128i_result[0]) = 0x0000002000000000;
  __m128i_out = __lsx_vclz_w (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080805;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080805;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m128i_result[0]) = 0x0808080808080808;
  __m128i_out = __lsx_vclz_b (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000000000;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[0]) = 0x0010001000100010;
  __m128i_out = __lsx_vclz_h (__m128i_op0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

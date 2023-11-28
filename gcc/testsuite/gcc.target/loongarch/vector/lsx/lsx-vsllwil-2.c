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
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000007f7f02;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00003f803f800100;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0014000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x7f807f807f807f80;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001030103;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0020006000200060;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0808080808080805;
  *((unsigned long *)&__m128i_op0[0]) = 0x0808080808080805;
  *((unsigned long *)&__m128i_result[1]) = 0x0020002000200020;
  *((unsigned long *)&__m128i_result[0]) = 0x0020002000200014;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001fffe0001fffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000201fe01fc;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000201fe01fc;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff1affff01001fe0;
  *((unsigned long *)&__m128i_op0[0]) = 0xff1aff6d02834d70;
  *((unsigned long *)&__m128i_result[1]) = 0x7f800d007f803680;
  *((unsigned long *)&__m128i_result[0]) = 0x0100418026803800;
  __m128i_out = __lsx_vsllwil_hu_bu (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3e2b34ca59fa4c88;
  *((unsigned long *)&__m128i_op0[0]) = 0x3b2c8aefd44be966;
  *((unsigned long *)&__m128i_result[1]) = 0x0007658000115de0;
  *((unsigned long *)&__m128i_result[0]) = 0x001a8960001d2cc0;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffff000000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000ff00;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000ff00;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000040600000406;
  *((unsigned long *)&__m128i_op0[0]) = 0x020202020202fe02;
  *((unsigned long *)&__m128i_result[1]) = 0x0020200000202000;
  *((unsigned long *)&__m128i_result[0]) = 0x002020000fe02000;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000001ffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000002;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3131313131313131;
  *((unsigned long *)&__m128i_result[1]) = 0x0313100003131000;
  *((unsigned long *)&__m128i_result[0]) = 0x0313100003131000;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000900000009;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000900000009;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000090;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000090;
  __m128i_out = __lsx_vsllwil_wu_hu (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000200000002;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000020000007d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000800000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000001f400000;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000280000;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000fef01000e27ca;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001fde020000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001c4f940000;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000ffffffff00;
  *((unsigned long *)&__m128i_result[0]) = 0x000000ffffffff00;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffff81010102;
  *((unsigned long *)&__m128i_result[1]) = 0x00000fffffffe000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000102020204000;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000800000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000008000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_w (__m128i_op0, 0x0);
  *((unsigned long *)&__m128i_op0[1]) = 0x8d78336c83652b86;
  *((unsigned long *)&__m128i_op0[0]) = 0x39c51f389c0d6112;
  *((unsigned long *)&__m128i_result[1]) = 0x00000001ce28f9c0;
  *((unsigned long *)&__m128i_result[0]) = 0x00000004e06b0890;
  __m128i_out = __lsx_vsllwil_du_wu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

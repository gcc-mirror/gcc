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
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xd82480697f678077;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfff489b693120950;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffc45a851c40c18;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000ff00;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000ff00;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1268f057137a0267;
  *((unsigned long *)&__m128i_op0[0]) = 0x0048137ef886fae0;
  *((unsigned long *)&__m128i_result[1]) = 0xff000000ff00ff00;
  *((unsigned long *)&__m128i_result[0]) = 0xff00ff0000000000;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000202fe02;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffff00ff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_bu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000001f0a;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffff7a53;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x40f0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x40f0001000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffffffffffff;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffff0000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0xffff0000ffff0000;
  *((unsigned long *)&__m128i_result[0]) = 0xffff0000ffff0000;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000000f;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffff0000;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00001b4a00007808;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffff0000ffff0000;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ffffffffffff;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_hu (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000003900;
  *((unsigned long *)&__m128i_op0[0]) = 0x68bcf93435ed25ed;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000001000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x03574e3a62407e03;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000001010000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000f0000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff000000ff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ff000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xa2a2a2a3a2a2a2a3;
  *((unsigned long *)&__m128i_op0[0]) = 0xc605c000aedd0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_wu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000ff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00250023001c001d;
  *((unsigned long *)&__m128i_op0[0]) = 0x309d2f342a5d2b34;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vslei_du (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

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
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f917d72d3d01e4;
  *((unsigned long *)&__m128i_op1[0]) = 0x203e16d116de012b;
  *((unsigned long *)&__m128i_result[1]) = 0x10f917d72d3d01e4;
  *((unsigned long *)&__m128i_result[0]) = 0x203e16d116de012b;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffebd06fffe820c;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fff7ffe7fff3506;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffebd06fffe820c;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fff7ffe7fff3506;
  *((unsigned long *)&__m128i_result[1]) = 0xffffff0cffffff18;
  *((unsigned long *)&__m128i_result[0]) = 0xfefffefffeff6a0c;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_op1[0]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffff60ca7104649;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffff790a15db63d;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0xfffff60ca710464a;
  *((unsigned long *)&__m128i_result[0]) = 0xfffff790a15db63e;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffff46;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00fe000100cf005f;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_op1[1]) = 0x5f675e96e29a5a60;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128i_result[1]) = 0x5fff5e97e2ff5abf;
  *((unsigned long *)&__m128i_result[0]) = 0xfefffefffefffeff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001000100010;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0001000100010058;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001001100110068;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x7fff010181010102;
  *((unsigned long *)&__m128i_op1[0]) = 0x7fffffff81010102;
  *((unsigned long *)&__m128i_result[1]) = 0xfeffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xfeffffffffffffff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ebd20000714f;
  *((unsigned long *)&__m128i_op0[0]) = 0x00012c8a0000a58a;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffb81a6f70;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000d48eaa1a2;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffb81ae0bf;
  *((unsigned long *)&__m128i_result[0]) = 0x00012c9748eaffff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0177fff0fffffff0;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000011ff8bc;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vsadd_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000200;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000001;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000d0000000d;
  *((unsigned long *)&__m128i_op1[1]) = 0x8006000000040000;
  *((unsigned long *)&__m128i_op1[0]) = 0x8002000000000007;
  *((unsigned long *)&__m128i_result[1]) = 0x8006000000040000;
  *((unsigned long *)&__m128i_result[0]) = 0x8002000d00000014;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000014;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  int_out = __lsx_vpickve2gr_h (__m128i_op0, 0x1);
  *((unsigned long *)&__m128i_op0[1]) = 0x0000000600007fff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000008ffffa209;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000600007fff;
  *((unsigned long *)&__m128i_result[0]) = 0x00000008ffffa209;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x636363633f3e47c1;
  *((unsigned long *)&__m128i_op0[0]) = 0x41f8e080f1ef4eaa;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000807bf0a1f80;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000800ecedee68;
  *((unsigned long *)&__m128i_result[1]) = 0x63636b6afe486741;
  *((unsigned long *)&__m128i_result[0]) = 0x41f8e880ffffffff;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ebd20000714f;
  *((unsigned long *)&__m128i_op0[0]) = 0x00012c8a0000a58a;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000ebd20000714f;
  *((unsigned long *)&__m128i_op1[0]) = 0x00012c8a0000a58a;
  *((unsigned long *)&__m128i_result[1]) = 0x0000ffff0000e29e;
  *((unsigned long *)&__m128i_result[0]) = 0x000259140000ffff;
  __m128i_out = __lsx_vsadd_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffffffeffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xfffffffeffffffff;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0c03e17edd781b11;
  *((unsigned long *)&__m128i_op0[0]) = 0x342caf9be55700b5;
  *((unsigned long *)&__m128i_op1[1]) = 0x00040003ff83ff84;
  *((unsigned long *)&__m128i_op1[0]) = 0x00040003ff4dffca;
  *((unsigned long *)&__m128i_result[1]) = 0x0c07e181ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x3430af9effffffff;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffa8ff9f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ffffffabff99;
  *((unsigned long *)&__m128i_op1[1]) = 0x000100000002007d;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000000020001;
  *((unsigned long *)&__m128i_result[1]) = 0x00010000ffab001c;
  *((unsigned long *)&__m128i_result[0]) = 0x0001ffffffadff9a;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_op1[0]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_result[1]) = 0x0800080008000800;
  *((unsigned long *)&__m128i_result[0]) = 0x0800080008000800;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x76f424887fffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xc110000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xc00d060000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xc110000000000001;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff7fffffff;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000002f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000029;
  *((unsigned long *)&__m128i_op1[1]) = 0xfbfbfb17fbfb38ea;
  *((unsigned long *)&__m128i_op1[0]) = 0xfbfb47fbfbfb0404;
  *((unsigned long *)&__m128i_result[1]) = 0xfbfbfb17fbfb3919;
  *((unsigned long *)&__m128i_result[0]) = 0xfbfb47fbfbfb042d;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808081;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x80808080ffffffff;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00123fff00120012;
  *((unsigned long *)&__m128i_op0[0]) = 0x0012001200120012;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x000000000005003a;
  *((unsigned long *)&__m128i_result[1]) = 0x00123fff00120012;
  *((unsigned long *)&__m128i_result[0]) = 0x001200120017004c;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xbfd10d0d7b6b6b73;
  *((unsigned long *)&__m128i_op1[0]) = 0xc5c534920000c4ed;
  *((unsigned long *)&__m128i_result[1]) = 0xbfd10d0d7b6b6b73;
  *((unsigned long *)&__m128i_result[0]) = 0xc5c534920000c4ed;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000aa822a79308f6;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_op1[1]) = 0x000aa822a79308f6;
  *((unsigned long *)&__m128i_op1[0]) = 0x03aa558e1d37b5a1;
  *((unsigned long *)&__m128i_result[1]) = 0x00155044ffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x03aa558e2584c86f;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x021b7d24c9678a35;
  *((unsigned long *)&__m128i_op0[0]) = 0x030298a6a1030a49;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x021b7d24c9678a35;
  *((unsigned long *)&__m128i_result[0]) = 0x030298a6a1030a49;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00007a8000000480;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000485000004cc;
  *((unsigned long *)&__m128i_op1[1]) = 0x00007a8000000480;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000485000004cc;
  *((unsigned long *)&__m128i_result[1]) = 0x0000f50000000900;
  *((unsigned long *)&__m128i_result[0]) = 0x0000090a00000998;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x004eff6200d2ff76;
  *((unsigned long *)&__m128i_op1[0]) = 0xff70002800be00a0;
  *((unsigned long *)&__m128i_result[1]) = 0x004eff6200d2ff76;
  *((unsigned long *)&__m128i_result[0]) = 0xff70002800be00a0;
  __m128i_out = __lsx_vsadd_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

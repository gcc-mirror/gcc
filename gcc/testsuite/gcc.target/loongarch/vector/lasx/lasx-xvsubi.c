/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
#include "../simd_correctness_check.h"
#include <lasxintrin.h>

int
main ()
{
  __m256i __m256i_op0, __m256i_op1, __m256i_op2, __m256i_out, __m256i_result;
  __m256 __m256_op0, __m256_op1, __m256_op2, __m256_out, __m256_result;
  __m256d __m256d_op0, __m256d_op1, __m256d_op2, __m256d_out, __m256d_result;

  int int_op0, int_op1, int_op2, int_out, int_result, i = 1, fail;
  long int long_op0, long_op1, long_op2, lont_out, lont_result;
  long int long_int_out, long_int_result;
  unsigned int unsigned_int_out, unsigned_int_result;
  unsigned long int unsigned_long_int_out, unsigned_long_int_result;

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m256i_result[2]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m256i_result[1]) = 0xe9e9e9e9e9e9e9e9;
  *((unsigned long *)&__m256i_result[0]) = 0xe9e9e9e9e9e9e9e9;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff00ff00000007;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000e7;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff00ff00000007;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[3]) = 0xf9f8f9f8f9f9f900;
  *((unsigned long *)&__m256i_result[2]) = 0xf9f9f9f9f9f9f9e0;
  *((unsigned long *)&__m256i_result[1]) = 0xf9f8f9f8f9f9f900;
  *((unsigned long *)&__m256i_result[0]) = 0xf9f9f9f9f9f9f900;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000000000007f;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xefefefefefefefef;
  *((unsigned long *)&__m256i_result[2]) = 0xefefefefefefefef;
  *((unsigned long *)&__m256i_result[1]) = 0xefefefefefefef6e;
  *((unsigned long *)&__m256i_result[0]) = 0xeeeeeeeeeeeeeeee;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[2]) = 0x6aeaeaeaeaeaeaea;
  *((unsigned long *)&__m256i_result[1]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[0]) = 0x6aeaeaeaeaeaeaea;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf6f6f6f6f6f6f6f6;
  *((unsigned long *)&__m256i_result[2]) = 0xf6f6f6f6f6f6f6f6;
  *((unsigned long *)&__m256i_result[1]) = 0xf6f6f6f6f6f6f6f6;
  *((unsigned long *)&__m256i_result[0]) = 0xf6f6f6f6f6f6f6f6;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000002a54290;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[2]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[1]) = 0xe7e7e7e7e7e7e7e7;
  *((unsigned long *)&__m256i_result[0]) = 0xe7e7e7e7e7e7e7e7;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xefdfefdf00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xefdfefdfefdfefdf;
  *((unsigned long *)&__m256i_op0[1]) = 0xefdfefdf00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xefdfefdfefdfefdf;
  *((unsigned long *)&__m256i_result[3]) = 0xdbcbdbcbecececec;
  *((unsigned long *)&__m256i_result[2]) = 0xdbcbdbcbdbcbdbcb;
  *((unsigned long *)&__m256i_result[1]) = 0xdbcbdbcbecececec;
  *((unsigned long *)&__m256i_result[0]) = 0xdbcbdbcbdbcbdbcb;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0e0d0c0b0e0d0c0b;
  *((unsigned long *)&__m256i_op0[2]) = 0x0e0d0c0b0e0d0c0b;
  *((unsigned long *)&__m256i_op0[1]) = 0x0e0d0c0b0e0d0c0b;
  *((unsigned long *)&__m256i_op0[0]) = 0x0e0d0c0b0e0d0c0b;
  *((unsigned long *)&__m256i_result[3]) = 0x0a0908070a090807;
  *((unsigned long *)&__m256i_result[2]) = 0x0a0908070a090807;
  *((unsigned long *)&__m256i_result[1]) = 0x0a0908070a090807;
  *((unsigned long *)&__m256i_result[0]) = 0x0a0908070a090807;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[2]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[1]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[0]) = 0xf9f9f9f9f9f9f9f9;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xf3f3f3f3f3f3f3f3;
  *((unsigned long *)&__m256i_result[2]) = 0xf2f2f2f2f2f2f2f2;
  *((unsigned long *)&__m256i_result[1]) = 0xf3f3f3f3f3f3f3f3;
  *((unsigned long *)&__m256i_result[0]) = 0xf2f2f2f2f2f2f2f2;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[2]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[1]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[0]) = 0xebebebebebebebeb;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_result[2]) = 0xfefefefefdfdfdfd;
  *((unsigned long *)&__m256i_result[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_result[0]) = 0xfefefefefdfdfdfd;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xe4e4e4e4e4e4e4e4;
  *((unsigned long *)&__m256i_result[2]) = 0xe4e4e4e4e4e4e4e4;
  *((unsigned long *)&__m256i_result[1]) = 0xe4e4e4e4e4e4e4e4;
  *((unsigned long *)&__m256i_result[0]) = 0xe4e4e4e4e4e4e4e4;
  __m256i_out = __lasx_xvsubi_bu (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff7fff7fff7fff7;
  *((unsigned long *)&__m256i_result[2]) = 0xfff7fff7fff7fff7;
  *((unsigned long *)&__m256i_result[1]) = 0xfff7fff7fff7fff7;
  *((unsigned long *)&__m256i_result[0]) = 0xfff7fff7fff7fff7;
  __m256i_out = __lasx_xvsubi_hu (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000022be22be;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff7fffa2bea2be;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000022be22be;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fffa2bea2be;
  *((unsigned long *)&__m256i_result[3]) = 0xffe1ffe1229f229f;
  *((unsigned long *)&__m256i_result[2]) = 0x7fe07fe0a29fa29f;
  *((unsigned long *)&__m256i_result[1]) = 0xffe1ffe1229f229f;
  *((unsigned long *)&__m256i_result[0]) = 0x7fe07fe0a29fa29f;
  __m256i_out = __lasx_xvsubi_hu (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffe5ffe5ffe5ffe5;
  *((unsigned long *)&__m256i_result[2]) = 0xffe5ffe5ffe5ffe5;
  *((unsigned long *)&__m256i_result[1]) = 0xffe5ffe5ffe5ffe5;
  *((unsigned long *)&__m256i_result[0]) = 0xffe5ffe5ffe5ffe5;
  __m256i_out = __lasx_xvsubi_hu (__m256i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff1fff1fff1fff1;
  *((unsigned long *)&__m256i_result[2]) = 0xfff1fff1fff1fff1;
  *((unsigned long *)&__m256i_result[1]) = 0xfff1fff1fff1fff1;
  *((unsigned long *)&__m256i_result[0]) = 0xfff1fff1fff1fff1;
  __m256i_out = __lasx_xvsubi_hu (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m256i_result[2]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m256i_result[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m256i_result[0]) = 0xfffcfffcfffcfffc;
  __m256i_out = __lasx_xvsubi_hu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000004fb;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffefffffffef;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffef000004ea;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffefffffffef;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffefffffffef;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffecffffffec;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffecffffffec;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffecffffffec;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffecffffffec;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000018;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000018;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff30000000b;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff3fffffff3;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff30000000b;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff3fffffff3;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff5fffffff5;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff5fffffff5;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff5fffffff5;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff5fffffff5;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffe5ffffffe5;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffe5ffffffe5;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffe5ffffffe5;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffe5ffffffe5;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffeaffffffea;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffeaffffffea;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffeaffffffea;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffeaffffffea;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x5d20a0a15d20a0a1;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x5d20a0a15d20a0a1;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x5d20a0895d20a089;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffe8ffffffe8;
  *((unsigned long *)&__m256i_result[1]) = 0x5d20a0895d20a089;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffe8ffffffe8;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffe8ffffffe8;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffe8ffffffe8;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffe8ffffffe8;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffe8ffffffe8;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffcfffffffc;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffcfffffffc;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffcfffffffc;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffcfffffffc;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffeb683007ffd80;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffe97c0df5b41cf;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffeb683007ffd80;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffe97c0df5b41cf;
  *((unsigned long *)&__m256i_result[3]) = 0xfffeb664007ffd61;
  *((unsigned long *)&__m256i_result[2]) = 0xfffe97a1df5b41b0;
  *((unsigned long *)&__m256i_result[1]) = 0xfffeb664007ffd61;
  *((unsigned long *)&__m256i_result[0]) = 0xfffe97a1df5b41b0;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffe7ffffffe7;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000400000003ffb;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000400100004001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000400000003ffb;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000400100004001;
  *((unsigned long *)&__m256i_result[3]) = 0x00003fef00003fea;
  *((unsigned long *)&__m256i_result[2]) = 0x00003ff000003ff0;
  *((unsigned long *)&__m256i_result[1]) = 0x00003fef00003fea;
  *((unsigned long *)&__m256i_result[0]) = 0x00003ff000003ff0;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffe4ffffffe4;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffe4ffffffe4;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffe4ffffffe4;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffe4ffffffe4;
  __m256i_out = __lasx_xvsubi_wu (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffefe;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000101;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffefe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000101;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffefb;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffefb;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000000fe;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffffc0008001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffffc0008001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffffc0008001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffffc0008001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000ffffc0007fe9;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ffffc0007fe9;
  *((unsigned long *)&__m256i_result[1]) = 0x0000ffffc0007fe9;
  *((unsigned long *)&__m256i_result[0]) = 0x0000ffffc0007fe9;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffff6;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff6;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffff6;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff6;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffee;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffe6;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffe6;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffe6;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffe6;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffe1;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffe1;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffe1;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffe1;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000100080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000100080;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000006d;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000010006d;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000006d;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000010006d;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffef;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffef;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffee;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff4;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffed;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffed;
  __m256i_out = __lasx_xvsubi_du (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

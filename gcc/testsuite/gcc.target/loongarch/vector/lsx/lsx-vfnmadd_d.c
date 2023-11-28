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

  *((unsigned long *)&__m128d_op0[1]) = 0xef0179a47c793879;
  *((unsigned long *)&__m128d_op0[0]) = 0x9f9e7e3e9ea3ff41;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128d_op2[0]) = 0x1e801ffc7fc00000;
  *((unsigned long *)&__m128d_result[1]) = 0xffc000007fc00000;
  *((unsigned long *)&__m128d_result[0]) = 0x9e801ffc7fc00000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x3f8000003f800000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000ffff00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000ffff00000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000ffff00000000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000008800022;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffff00000001;
  *((unsigned long *)&__m128d_op2[1]) = 0xb8ec43befe38e64b;
  *((unsigned long *)&__m128d_op2[0]) = 0x6477d042343cce24;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffbfffffffbf;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffbfffffffbf;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffffffffffff000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000060000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xfffffffffffff000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xfffffffafffffffa;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffffffafffffffa;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m128d_op1[0]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000008000000080;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000008000000080;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xff80ffa2fff0ff74;
  *((unsigned long *)&__m128d_op0[0]) = 0xff76ffd8ffe6ffaa;
  *((unsigned long *)&__m128d_op1[1]) = 0xff80ffa2fff0ff74;
  *((unsigned long *)&__m128d_op1[0]) = 0xff76ffd8ffe6ffaa;
  *((unsigned long *)&__m128d_op2[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128d_op2[0]) = 0x0303030303030303;
  *((unsigned long *)&__m128d_result[1]) = 0xfff0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xfff0000000000000;
  __m128d_out = __lsx_vfnmadd_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0001ffff00000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0001ffff0001ffff;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m128d_op1[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x3c600000ff800000;
  *((unsigned long *)&__m128d_result[0]) = 0xfffffffffffffffe;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x000000000000000d;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x00000000b5207f80;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x00000000b5207f80;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffffffffff;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000009000900;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000009000900;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x8000000000000000;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x00c2758000bccf42;
  *((unsigned long *)&__m128d_op0[0]) = 0x00a975be00accf03;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128d_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op2[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x00000000ffffffff;
  __m128d_out = __lsx_vfnmsub_d (__m128d_op0, __m128d_op1, __m128d_op2);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}

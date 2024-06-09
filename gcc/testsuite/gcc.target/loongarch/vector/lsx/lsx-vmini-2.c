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

  *((unsigned long *)&__m128i_op0[1]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000a163000016b0;
  *((unsigned long *)&__m128i_result[1]) = 0x0303000103030001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000030300000303;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xd8248069ffe78077;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0d0d0d0d0d0d0d0d;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7da9b23a624082fd;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0505050505050505;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000005050000;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001300000013;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000e0000000e;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000e0000000e;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0xe);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000001fffdfffdff;
  *((unsigned long *)&__m128i_op0[0]) = 0x000001fffdfffdff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000010101010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0000010101010101;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000009c007c00;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000071007600;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000009000900;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000009000900;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xc0b4d1a5f8babad3;
  *((unsigned long *)&__m128i_op0[0]) = 0xbbc8ecc5f3ced5f3;
  *((unsigned long *)&__m128i_result[1]) = 0x0303030303030303;
  *((unsigned long *)&__m128i_result[0]) = 0x0303030303030303;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xd3220000d3f20000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8bff0000a7b80000;
  *((unsigned long *)&__m128i_result[1]) = 0x0909000009090000;
  *((unsigned long *)&__m128i_result[0]) = 0x0909000009090000;
  __m128i_out = __lsx_vmini_bu (__m128i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x80000000b57ec564;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000083ff0be0;
  *((unsigned long *)&__m128i_result[1]) = 0x0014000000140014;
  *((unsigned long *)&__m128i_result[0]) = 0x0014000000140014;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_op0[0]) = 0x1000100010001000;
  *((unsigned long *)&__m128i_result[1]) = 0x0013001300130013;
  *((unsigned long *)&__m128i_result[0]) = 0x0013001300130013;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000005;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0x1b);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_op0[0]) = 0x27b169bbb8145f50;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_result[0]) = 0x0002000200020002;
  __m128i_out = __lsx_vmini_hu (__m128i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000040004000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0010002000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ff00ff0000007f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000004;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ff8000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000005;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000003fc00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001fe01fe00;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000a;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000a;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fc000007fc00000;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000000b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000000b;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000101010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000014;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x14);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000005;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000005;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000d3460001518a;
  *((unsigned long *)&__m128i_op0[0]) = 0x000084300000e55f;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000016;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000016;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffff2356fe165486;
  *((unsigned long *)&__m128i_op0[0]) = 0x5efeb3165bd7653d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000007;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000007;
  __m128i_out = __lsx_vmini_du (__m128i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

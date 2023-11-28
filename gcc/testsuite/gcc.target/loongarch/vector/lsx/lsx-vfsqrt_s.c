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

  *((int *)&__m128_op0[3]) = 0xfe07e5fe;
  *((int *)&__m128_op0[2]) = 0xfefdddfe;
  *((int *)&__m128_op0[1]) = 0x00020100;
  *((int *)&__m128_op0[0]) = 0xfedd0c00;
  *((int *)&__m128_result[3]) = 0x7fc00000;
  *((int *)&__m128_result[2]) = 0x7fc00000;
  *((int *)&__m128_result[1]) = 0x1e801ffc;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xff00ff00;
  *((int *)&__m128_op0[2]) = 0xff00ff00;
  *((int *)&__m128_op0[1]) = 0xff00ff00;
  *((int *)&__m128_op0[0]) = 0xff00ff00;
  *((int *)&__m128_result[3]) = 0x7fc00000;
  *((int *)&__m128_result[2]) = 0x7fc00000;
  *((int *)&__m128_result[1]) = 0x7fc00000;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x8c7fc73a;
  *((int *)&__m128_op0[2]) = 0x137e54af;
  *((int *)&__m128_op0[1]) = 0xbc84cf6f;
  *((int *)&__m128_op0[0]) = 0x76208329;
  *((int *)&__m128_result[3]) = 0x7fc00000;
  *((int *)&__m128_result[2]) = 0x297f29fe;
  *((int *)&__m128_result[1]) = 0x7fc00000;
  *((int *)&__m128_result[0]) = 0x5acab5a5;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffff9727;
  *((int *)&__m128_op0[2]) = 0xffff9727;
  *((int *)&__m128_op0[1]) = 0xfffffe79;
  *((int *)&__m128_op0[0]) = 0xffffba5f;
  *((int *)&__m128_result[3]) = 0xffff9727;
  *((int *)&__m128_result[2]) = 0xffff9727;
  *((int *)&__m128_result[1]) = 0xfffffe79;
  *((int *)&__m128_result[0]) = 0xffffba5f;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xfff8fff8;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xfff80000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0xfff8fff8;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0xfff80000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0xffffffff;
  *((int *)&__m128_result[0]) = 0xffffffff;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x1f1b917c;
  *((int *)&__m128_op0[0]) = 0x9f3d5e05;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x4fa432d6;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x12835580;
  *((int *)&__m128_op0[0]) = 0xb880eb98;
  *((int *)&__m128_result[3]) = 0xffffffff;
  *((int *)&__m128_result[2]) = 0xffffffff;
  *((int *)&__m128_result[1]) = 0x55fcbad1;
  *((int *)&__m128_result[0]) = 0x7fc00000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x06070607;
  *((int *)&__m128_op0[2]) = 0x00000807;
  *((int *)&__m128_op0[1]) = 0x0707f8f8;
  *((int *)&__m128_op0[0]) = 0x03e8157e;
  *((int *)&__m128_result[3]) = 0x5c303f97;
  *((int *)&__m128_result[2]) = 0x61ff9049;
  *((int *)&__m128_result[1]) = 0x5bafa1dd;
  *((int *)&__m128_result[0]) = 0x5d3e1e1d;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xfff7fffe;
  *((int *)&__m128_op0[2]) = 0xfffa01ff;
  *((int *)&__m128_op0[1]) = 0xfffbfffe;
  *((int *)&__m128_op0[0]) = 0xfffe01ff;
  *((int *)&__m128_result[3]) = 0xfff7fffe;
  *((int *)&__m128_result[2]) = 0xfffa01ff;
  *((int *)&__m128_result[1]) = 0xfffbfffe;
  *((int *)&__m128_result[0]) = 0xfffe01ff;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x45000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x44000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x3cb504f3;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x3d3504f3;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00020001;
  *((int *)&__m128_op0[0]) = 0x00020002;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x607fffc0;
  *((int *)&__m128_result[0]) = 0x607fff80;
  __m128_out = __lsx_vfrsqrt_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000002;
  *((int *)&__m128_op0[2]) = 0x00000002;
  *((int *)&__m128_op0[1]) = 0x00000003;
  *((int *)&__m128_op0[0]) = 0x00000003;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xf6e91c00;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x51cfd7c0;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x880c91b8;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x2d1da85b;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xfffffffa;
  *((int *)&__m128_op0[2]) = 0xfffffffa;
  *((int *)&__m128_op0[1]) = 0xfffffffa;
  *((int *)&__m128_op0[0]) = 0xfffffffa;
  *((int *)&__m128_result[3]) = 0xfffffffa;
  *((int *)&__m128_result[2]) = 0xfffffffa;
  *((int *)&__m128_result[1]) = 0xfffffffa;
  *((int *)&__m128_result[0]) = 0xfffffffa;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffff0001;
  *((int *)&__m128_op0[2]) = 0xffff0001;
  *((int *)&__m128_op0[1]) = 0xffff0001;
  *((int *)&__m128_op0[0]) = 0xffff0001;
  *((int *)&__m128_result[3]) = 0xffff0001;
  *((int *)&__m128_result[2]) = 0xffff0001;
  *((int *)&__m128_result[1]) = 0xffff0001;
  *((int *)&__m128_result[0]) = 0xffff0001;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x0a000000;
  *((int *)&__m128_op0[2]) = 0x0a000000;
  *((int *)&__m128_op0[1]) = 0x0a000000;
  *((int *)&__m128_op0[0]) = 0x0a000000;
  *((int *)&__m128_result[3]) = 0x75000000;
  *((int *)&__m128_result[2]) = 0x75000000;
  *((int *)&__m128_result[1]) = 0x75000000;
  *((int *)&__m128_result[0]) = 0x75000000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_result[3]) = 0x7f800000;
  *((int *)&__m128_result[2]) = 0x7f800000;
  *((int *)&__m128_result[1]) = 0x7f800000;
  *((int *)&__m128_result[0]) = 0x7f800000;
  __m128_out = __lsx_vfrecip_s (__m128_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  return 0;
}

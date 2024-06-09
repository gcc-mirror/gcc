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
  *((unsigned long *)&__m128i_op0[0]) = 0x00e0000000e00000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000002a55005501;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000002a55000001;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x36280000;
  *((int *)&__m128_result[1]) = 0x42a00000;
  *((int *)&__m128_result[0]) = 0x42a02000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xf436f3f5;
  *((int *)&__m128_op0[0]) = 0x2f4ef4a8;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffcfb799f1;
  *((unsigned long *)&__m128i_op0[0]) = 0x0282800002828282;
  *((int *)&__m128_result[3]) = 0xffffe000;
  *((int *)&__m128_result[2]) = 0xffffe000;
  *((int *)&__m128_result[1]) = 0xc1f6e000;
  *((int *)&__m128_result[0]) = 0xbb3e2000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000040004000100;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000006f00001f0a;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000958affff995d;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x36de0000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x3be14000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x41dfffff00000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x403be000;
  *((int *)&__m128_result[2]) = 0xffffe000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x63637687;
  *((int *)&__m128_op0[2]) = 0x636316bb;
  *((int *)&__m128_op0[1]) = 0x63636363;
  *((int *)&__m128_op0[0]) = 0x63636363;
  *((unsigned long *)&__m128d_result[1]) = 0x446c6ed0e0000000;
  *((unsigned long *)&__m128d_result[0]) = 0x446c62d760000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x000000ff;
  *((int *)&__m128_op0[2]) = 0x000000ff;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x371fe00000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x371fe00000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffff7fff7ef;
  *((unsigned long *)&__m128i_op0[0]) = 0x80808080ffffffff;
  *((int *)&__m128_result[3]) = 0xffffe000;
  *((int *)&__m128_result[2]) = 0xffffe000;
  *((int *)&__m128_result[1]) = 0xc6ffe000;
  *((int *)&__m128_result[0]) = 0xc6fde000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0xffffffffe0000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffe0000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffe1ffc100000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000400000;
  *((int *)&__m128_result[3]) = 0xfffc2000;
  *((int *)&__m128_result[2]) = 0xfff82000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x0000b3a6;
  *((int *)&__m128_op0[2]) = 0x000067da;
  *((int *)&__m128_op0[1]) = 0x00004e42;
  *((int *)&__m128_op0[0]) = 0x0000c26a;
  *((unsigned long *)&__m128d_result[1]) = 0x379674c000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x3789f68000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0xffff0000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xffffffffe0000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001001001000080;
  *((unsigned long *)&__m128i_op0[0]) = 0x4195d926d8018000;
  *((int *)&__m128_result[3]) = 0x33800000;
  *((int *)&__m128_result[2]) = 0x35800000;
  *((int *)&__m128_result[1]) = 0x37800000;
  *((int *)&__m128_result[0]) = 0x37000000;
  __m128_out = __lsx_vfcvth_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvth_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((int *)&__m128_result[3]) = 0xffffe000;
  *((int *)&__m128_result[2]) = 0xffffe000;
  *((int *)&__m128_result[1]) = 0xffffe000;
  *((int *)&__m128_result[0]) = 0xffffe000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000a000a000a000a;
  *((unsigned long *)&__m128i_op0[0]) = 0x000a000a000a000a;
  *((int *)&__m128_result[3]) = 0x35200000;
  *((int *)&__m128_result[2]) = 0x35200000;
  *((int *)&__m128_result[1]) = 0x35200000;
  *((int *)&__m128_result[0]) = 0x35200000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000100;
  *((int *)&__m128_op0[2]) = 0x0f00fe00;
  *((int *)&__m128_op0[1]) = 0x0000017f;
  *((int *)&__m128_op0[0]) = 0xff00fe7f;
  *((unsigned long *)&__m128d_result[1]) = 0x3727f00000000000;
  *((unsigned long *)&__m128d_result[0]) = 0xc7e01fcfe0000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000020;
  *((int *)&__m128_op0[0]) = 0x00000020;
  *((unsigned long *)&__m128d_result[1]) = 0x36f0000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x36f0000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xbd994889;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x0a092444;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x3941248880000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x62cbf96e4acfaf40;
  *((unsigned long *)&__m128i_op0[0]) = 0xf0bc9a5278285a4a;
  *((int *)&__m128_result[3]) = 0xc6178000;
  *((int *)&__m128_result[2]) = 0xbb4a4000;
  *((int *)&__m128_result[1]) = 0x47050000;
  *((int *)&__m128_result[0]) = 0x43494000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00040004;
  *((int *)&__m128_op0[2]) = 0x00040004;
  *((int *)&__m128_op0[1]) = 0x00040004;
  *((int *)&__m128_op0[0]) = 0x00040004;
  *((unsigned long *)&__m128d_result[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x37c0001000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0xff00ff00ff00ff00;
  *((int *)&__m128_result[3]) = 0xffe00000;
  *((int *)&__m128_result[2]) = 0xffe00000;
  *((int *)&__m128_result[1]) = 0xffe00000;
  *((int *)&__m128_result[0]) = 0xffe00000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0x00000000;
  *((int *)&__m128_result[0]) = 0x00000000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000ffffffff;
  *((int *)&__m128_result[3]) = 0x00000000;
  *((int *)&__m128_result[2]) = 0x00000000;
  *((int *)&__m128_result[1]) = 0xffffe000;
  *((int *)&__m128_result[0]) = 0xffffe000;
  __m128_out = __lsx_vfcvtl_s_h (__m128i_op0);
  ASSERTEQ_32 (__LINE__, __m128_result, __m128_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x0000000000000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x007f7f7f;
  *((unsigned long *)&__m128d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_result[0]) = 0x380fdfdfc0000000;
  __m128d_out = __lsx_vfcvtl_d_s (__m128_op0);
  ASSERTEQ_64 (__LINE__, __m128d_result, __m128d_out);

  return 0;
}

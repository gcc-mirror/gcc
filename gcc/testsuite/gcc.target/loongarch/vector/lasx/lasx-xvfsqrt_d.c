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

  *((unsigned long *)&__m256d_op0[3]) = 0x1e1800001e180000;
  *((unsigned long *)&__m256d_op0[2]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x1e1800001e180000;
  *((unsigned long *)&__m256d_op0[0]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x2f03988e2052463e;
  *((unsigned long *)&__m256d_result[2]) = 0x2f03988e1409212e;
  *((unsigned long *)&__m256d_result[1]) = 0x2f03988e2052463e;
  *((unsigned long *)&__m256d_result[0]) = 0x2f03988e1409212e;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00000000003f7e3f;
  *((unsigned long *)&__m256d_op0[2]) = 0xffc6cc05c64d960e;
  *((unsigned long *)&__m256d_op0[1]) = 0x00000000003f7e3f;
  *((unsigned long *)&__m256d_op0[0]) = 0xff874dc687870000;
  *((unsigned long *)&__m256d_result[3]) = 0x1f0fdf7f3e3b31d4;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff8000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x1f0fdf7f3e3b31d4;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff8000000000000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000100000018;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000100000018;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x1f60000000c00000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x1f60000000c00000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0003030300000300;
  *((unsigned long *)&__m256d_op0[2]) = 0x0003030300000300;
  *((unsigned long *)&__m256d_op0[1]) = 0x0003030300000100;
  *((unsigned long *)&__m256d_op0[0]) = 0x0003030300000100;
  *((unsigned long *)&__m256d_result[3]) = 0x1febc46085090ea0;
  *((unsigned long *)&__m256d_result[2]) = 0x1febc46085090ea0;
  *((unsigned long *)&__m256d_result[1]) = 0x1febc46085090567;
  *((unsigned long *)&__m256d_result[0]) = 0x1febc46085090567;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000007f007f007f;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000007f007f007f;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x1f9689fdb16cabbd;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x1f9689fdb16cabbd;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffff0000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffff0000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffff0000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffff0000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000010000000100;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000010000000100;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x1fa0000000080000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffff8000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffff8000;
  __m256d_out = __lasx_xvfsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffff00000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x000000000000ffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x0209fefb08140000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0003fffc00060000;
  *((unsigned long *)&__m256d_result[3]) = 0x6100000800060005;
  *((unsigned long *)&__m256d_result[2]) = 0x5ee1c073b800c916;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x5ff00007fff9fff3;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x555555553f800000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x555555553f800000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x353bb67af686ad9b;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x353bb67af686ad9b;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000001f0000001f;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000001f0000ffff;
  *((unsigned long *)&__m256d_result[3]) = 0x60000007fffe0001;
  *((unsigned long *)&__m256d_result[2]) = 0x60000007fffe0001;
  *((unsigned long *)&__m256d_result[1]) = 0x6056fd4e7926d5c0;
  *((unsigned long *)&__m256d_result[0]) = 0x6056fd4e1a4616c4;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00001bfa000000f9;
  *((unsigned long *)&__m256d_op0[2]) = 0x000000f900004040;
  *((unsigned long *)&__m256d_op0[1]) = 0x00001bfa000000f9;
  *((unsigned long *)&__m256d_op0[0]) = 0x000000f900004040;
  *((unsigned long *)&__m256d_result[3]) = 0x60183329ceb52cf0;
  *((unsigned long *)&__m256d_result[2]) = 0x6040392cdaf9b3ff;
  *((unsigned long *)&__m256d_result[1]) = 0x60183329ceb52cf0;
  *((unsigned long *)&__m256d_result[0]) = 0x6040392cdaf9b3ff;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x3de00103153ff5fb;
  *((unsigned long *)&__m256d_op0[2]) = 0xbffffffe80000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x3de00103153ff5fb;
  *((unsigned long *)&__m256d_op0[0]) = 0xbffffffe80000000;
  *((unsigned long *)&__m256d_result[3]) = 0x40f69fe73c26f4ee;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff8000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x40f69fe73c26f4ee;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff8000000000000;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00000005ffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x00000007ffffffce;
  *((unsigned long *)&__m256d_op0[1]) = 0x00000005ffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x00000007ffffffce;
  *((unsigned long *)&__m256d_result[3]) = 0x606a20bd700e59a3;
  *((unsigned long *)&__m256d_result[2]) = 0x6066a09e66c5f1bb;
  *((unsigned long *)&__m256d_result[1]) = 0x606a20bd700e59a3;
  *((unsigned long *)&__m256d_result[0]) = 0x6066a09e66c5f1bb;
  __m256d_out = __lasx_xvfrsqrt_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x03fc03fc03f803f8;
  *((unsigned long *)&__m256d_op0[2]) = 0x03fc03fc03f803f8;
  *((unsigned long *)&__m256d_op0[1]) = 0x03fc03fc03f803f8;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7be2468acf15f39c;
  *((unsigned long *)&__m256d_result[2]) = 0x7be2468acf15f39c;
  *((unsigned long *)&__m256d_result[1]) = 0x7be2468acf15f39c;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000002a54290;
  *((unsigned long *)&__m256d_op0[2]) = 0x000000000154dc84;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000002a54290;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000089;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256d_op0[2]) = 0xd0d8eecf383fdf0d;
  *((unsigned long *)&__m256d_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256d_op0[0]) = 0xd0d8eecf383fdf0d;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xaf0489001bd4c0c3;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xaf0489001bd4c0c3;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000a00000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x00000000fffff614;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000a00000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x00000000fffff614;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000001e0000001e;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000001e0000001e;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000001e0000001e;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000001e0000001e;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xff80000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xff80000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x8060000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x8060000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrecip_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  return 0;
}

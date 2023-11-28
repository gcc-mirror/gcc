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
  int_op1 = 0x00000045eef14fe8;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000080000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000000000ac;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x78c00000ff000000;
  int_op1 = 0x0000000000000400;
  *((unsigned long *)&__m128i_result[1]) = 0xff000000ff000000;
  *((unsigned long *)&__m128i_result[0]) = 0xff000000ff000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x803f800080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xe0404041c0404040;
  int_op1 = 0x0000000000000001;
  *((unsigned long *)&__m128i_result[1]) = 0xe0404041e0404041;
  *((unsigned long *)&__m128i_result[0]) = 0xe0404041e0404041;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000ffffffff0001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000100010001;
  int_op1 = 0x3f8000003f800000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000100010001;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000020006;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffb4ff;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffb4ff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffb4ff;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000020202020;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x000000007ff00000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000020006;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffffffffff4;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00ffff00ff00ff00;
  *((unsigned long *)&__m128i_op0[0]) = 0x00ffff00ff00ff00;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m128i_result[0]) = 0xff00ff00ff00ff00;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000080000000;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m128i_result[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[0]) = 0x8080808080808080;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7ff0000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000001b;
  int_op1 = 0xffffffff89e08000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000001b0000001b;
  *((unsigned long *)&__m128i_result[0]) = 0x0000001b0000001b;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfefefefdbffefdfe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfefefeeffef7fefe;
  int_op1 = 0xffffffff9c0d6112;
  *((unsigned long *)&__m128i_result[1]) = 0xbffefdfebffefdfe;
  *((unsigned long *)&__m128i_result[0]) = 0xbffefdfebffefdfe;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_op0[0]) = 0xff800000ff800000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m128i_result[0]) = 0xff800000ff800000;
  __m128i_out = __lsx_vreplve_w (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op0[0]) = 0xffd27db010d20fbf;
  int_op1 = 0x0000000000000040;
  *((unsigned long *)&__m128i_result[1]) = 0x0fbf0fbf0fbf0fbf;
  *((unsigned long *)&__m128i_result[0]) = 0x0fbf0fbf0fbf0fbf;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x00000000090b0906;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_b (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  int_op1 = 0xffffffffffff8a35;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x05dfffc3ffffffc0;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000047fe2f0;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000047fe2f0;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000047fe2f0;
  __m128i_out = __lsx_vreplve_d (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffefffefffefffe;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffefffe011df03e;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xf03ef03ef03ef03e;
  *((unsigned long *)&__m128i_result[0]) = 0xf03ef03ef03ef03e;
  __m128i_out = __lsx_vreplve_h (__m128i_op0, int_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

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

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xe17fe003;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0x0000ffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0xffffffff;
  *((int *)&__m128_op1[2]) = 0xffffffff;
  *((int *)&__m128_op1[1]) = 0xffffffff;
  *((int *)&__m128_op1[0]) = 0xffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xffffffff;
  *((int *)&__m128_op0[0]) = 0xffffffff;
  *((int *)&__m128_op1[3]) = 0x00190819;
  *((int *)&__m128_op1[2]) = 0x00190019;
  *((int *)&__m128_op1[1]) = 0x00190819;
  *((int *)&__m128_op1[0]) = 0x00190019;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xfe800000;
  *((int *)&__m128_op0[0]) = 0x00000001;
  *((int *)&__m128_op1[3]) = 0x7fffffff;
  *((int *)&__m128_op1[2]) = 0x82bb9784;
  *((int *)&__m128_op1[1]) = 0x7fffffff;
  *((int *)&__m128_op1[0]) = 0xc6bb97ac;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x7f3f0180;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0xa2321469;
  *((int *)&__m128_op0[0]) = 0x7fd03f7f;
  *((int *)&__m128_op1[3]) = 0x00000406;
  *((int *)&__m128_op1[2]) = 0x00000406;
  *((int *)&__m128_op1[1]) = 0x02020202;
  *((int *)&__m128_op1[0]) = 0x0202fe02;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0xfffffff5;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000ffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000000;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x00000000;
  *((int *)&__m128_op0[2]) = 0x00000014;
  *((int *)&__m128_op0[1]) = 0x00000000;
  *((int *)&__m128_op0[0]) = 0x00000014;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0xfffc0004;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffff00000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x67eb85b0;
  *((int *)&__m128_op0[2]) = 0xb2ebb001;
  *((int *)&__m128_op0[1]) = 0xc8847ef6;
  *((int *)&__m128_op0[0]) = 0xed3f2000;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x00000000;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0x0002de46;
  *((int *)&__m128_op0[2]) = 0x682de060;
  *((int *)&__m128_op0[1]) = 0x09b50da6;
  *((int *)&__m128_op0[0]) = 0xe67b8fc0;
  *((int *)&__m128_op1[3]) = 0x00000000;
  *((int *)&__m128_op1[2]) = 0x084d12ce;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x24170000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0x00ffffff;
  *((int *)&__m128_op0[0]) = 0x00ffffff;
  *((int *)&__m128_op1[3]) = 0x0000feff;
  *((int *)&__m128_op1[2]) = 0x23560000;
  *((int *)&__m128_op1[1]) = 0x0000fd16;
  *((int *)&__m128_op1[0]) = 0x54860000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((int *)&__m128_op0[3]) = 0xffffffff;
  *((int *)&__m128_op0[2]) = 0xffffffff;
  *((int *)&__m128_op0[1]) = 0xfffffffe;
  *((int *)&__m128_op0[0]) = 0xfffff784;
  *((int *)&__m128_op1[3]) = 0x0177fff0;
  *((int *)&__m128_op1[2]) = 0xfffffff0;
  *((int *)&__m128_op1[1]) = 0x00000000;
  *((int *)&__m128_op1[0]) = 0x011ff8bc;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_s (__m128_op0, __m128_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0002000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0002000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128d_op1[0]) = 0x00ff00ff00ff00fe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffe5;
  *((unsigned long *)&__m128d_op1[0]) = 0xffffffffffffffe5;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m128d_op0[0]) = 0x000000007fff7fff;
  *((unsigned long *)&__m128d_op1[1]) = 0xfffffffffffff800;
  *((unsigned long *)&__m128d_op1[0]) = 0x00ff000000ff0000;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xfffebd06fffe820c;
  *((unsigned long *)&__m128d_op1[0]) = 0x7fff7ffe7fff3506;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128d_op1[0]) = 0x7ffffffeffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[0]) = 0xffffffffffffffff;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128d_op0[0]) = 0x4f804f804f804f80;
  *((unsigned long *)&__m128d_op1[1]) = 0x00007fff00007fff;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x000000000000002f;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000029;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000003a24;
  *((unsigned long *)&__m128d_op1[0]) = 0x003dbe88077c78c1;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128d_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m128d_op0[0]) = 0x0001fffe0001fefc;
  *((unsigned long *)&__m128d_op1[1]) = 0x0007000000050000;
  *((unsigned long *)&__m128d_op1[0]) = 0x0003000100010001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfcmp_sun_d (__m128d_op0, __m128d_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

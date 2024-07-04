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

  *((unsigned long *)&__m256i_op0[3]) = 0x8b1414140e0e0e0e;
  *((unsigned long *)&__m256i_op0[2]) = 0x146014141414146e;
  *((unsigned long *)&__m256i_op0[1]) = 0x36722a7e66972cd6;
  *((unsigned long *)&__m256i_op0[0]) = 0xf19998668e5f4b84;
  long_op1 = 0x0000007942652524;
  *((unsigned long *)&__m256i_result[3]) = 0x8b1414140e0e0e0e;
  *((unsigned long *)&__m256i_result[2]) = 0x0000007942652524;
  *((unsigned long *)&__m256i_result[1]) = 0x36722a7e66972cd6;
  *((unsigned long *)&__m256i_result[0]) = 0xf19998668e5f4b84;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0202020201010000;
  int_op1 = 0x00000045eef14fe8;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000eef14fe8;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0202020201010000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000200000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x012e2110012e2110;
  int_op1 = 0x00000000000000ac;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000200000000;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000000000ac;
  *((unsigned long *)&__m256i_result[0]) = 0x012e2110012e2110;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000000000000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_op0[2]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_op0[0]) = 0xff800000ff800000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_result[2]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_result[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_result[0]) = 0xff80000000000000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffff0000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffff0000ff;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffff0000ff;
  *((unsigned long *)&__m256i_result[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffff0000ff;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xe800c000fffeeece;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff383efffedf0c;
  *((unsigned long *)&__m256i_op0[1]) = 0xe800c000fffeeece;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff383efffedf0c;
  int_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0xe800c000fffeeece;
  *((unsigned long *)&__m256i_result[2]) = 0xffff383e000000ff;
  *((unsigned long *)&__m256i_result[1]) = 0xe800c000fffeeece;
  *((unsigned long *)&__m256i_result[0]) = 0xffff383efffedf0c;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_op0[2]) = 0x0020000000200000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_op0[0]) = 0x0020000000200000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_result[2]) = 0x0020000000200000;
  *((unsigned long *)&__m256i_result[1]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[1]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_op0[0]) = 0x4040404040404040;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[2]) = 0x4040404040404040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000040404040;
  *((unsigned long *)&__m256i_result[0]) = 0x4040404040404040;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000048;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000048;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000048;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_op1 = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffff7fffffff7;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff700000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff7fffffff7;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_d (__m256i_op0, long_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000d6d6d;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000d6d6d;
  int_op1 = 0x00000000090b0906;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000090b0906;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000d6d6d;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_op1 = 0x000000000000001e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001e00000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op0[2]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_op0[1]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op0[0]) = 0xfefefefefefefefe;
  int_op1 = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[2]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000050005;
  *((unsigned long *)&__m256i_result[0]) = 0xfefefefefefefefe;
  __m256i_out = __lasx_xvinsgr2vr_w (__m256i_op0, int_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

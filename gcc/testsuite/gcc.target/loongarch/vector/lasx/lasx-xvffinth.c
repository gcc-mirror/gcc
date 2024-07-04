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

  *((unsigned long *)&__m256i_op0[3]) = 0x0e2d5626ff75cdbc;
  *((unsigned long *)&__m256i_op0[2]) = 0x5db4b156e2002a78;
  *((unsigned long *)&__m256i_op0[1]) = 0xeeffbeb03ba3e6b0;
  *((unsigned long *)&__m256i_op0[0]) = 0x0c16e25eb28d27ea;
  *((unsigned long *)&__m256d_result[3]) = 0x41ac5aac4c000000;
  *((unsigned long *)&__m256d_result[2]) = 0xc161464880000000;
  *((unsigned long *)&__m256d_result[1]) = 0xc1b1004150000000;
  *((unsigned long *)&__m256d_result[0]) = 0x41cdd1f358000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000006f0000007f;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000006f0000007f;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xe161616161616161;
  *((unsigned long *)&__m256i_op0[2]) = 0xe161616161614e60;
  *((unsigned long *)&__m256i_op0[1]) = 0xe161616161616161;
  *((unsigned long *)&__m256i_op0[0]) = 0xe161616161614e60;
  *((unsigned long *)&__m256d_result[3]) = 0xc1be9e9e9f000000;
  *((unsigned long *)&__m256d_result[2]) = 0x41d8585858400000;
  *((unsigned long *)&__m256d_result[1]) = 0xc1be9e9e9f000000;
  *((unsigned long *)&__m256d_result[0]) = 0x41d8585858400000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fff7fff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff00000000;
  *((unsigned long *)&__m256d_result[3]) = 0x41dfffc000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x41dfffdfffc00000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000007f3a40;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffb79fb74;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffb79fb74;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m256d_result[3]) = 0xbff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xc192181230000000;
  *((unsigned long *)&__m256d_result[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xc192181230000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xbff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xbff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xbff0000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000ffffffff00;
  *((unsigned long *)&__m256d_result[3]) = 0x40efffe000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x40efffe000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x41dffc0000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x41dffc0000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffe7ffffffe7;
  *((unsigned long *)&__m256d_result[3]) = 0xc039000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xc039000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xc039000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xc039000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffinth_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x5980000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x5980000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x41d6600000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x41d6600000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000022beb03f;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffa2beb040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000022beb03f;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffa2beb040;
  *((unsigned long *)&__m256d_result[3]) = 0x41dfffffffc00000;
  *((unsigned long *)&__m256d_result[2]) = 0xc1d75053f0000000;
  *((unsigned long *)&__m256d_result[1]) = 0x41dfffffffc00000;
  *((unsigned long *)&__m256d_result[0]) = 0xc1d75053f0000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000001f;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000001f;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x403f000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x403f000000000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00f7000000f70006;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00f7000000f70006;
  *((unsigned long *)&__m256d_result[3]) = 0x416ee00000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x416ee000c0000000;
  *((unsigned long *)&__m256d_result[1]) = 0x416ee00000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x416ee000c0000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00ff000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00ff000000000080;
  *((unsigned long *)&__m256d_result[3]) = 0x416fe00000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x4060000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x416fe00000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x4060000000000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffc01fc01;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000003fc03bbc;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffc01fc01;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000003fc03bbc;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x41cfe01dde000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x41cfe01dde000000;
  __m256d_out = __lasx_xvffintl_d_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  return 0;
}

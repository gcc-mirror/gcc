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

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[3]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_result[2]) = 0x45c5c5c545c5c5c5;
  *((unsigned long *)&__m256i_result[1]) = 0xc5c5c5c4c5c5c5c4;
  *((unsigned long *)&__m256i_result[0]) = 0x45c5c5c545c5c5c5;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x3a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000007773;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000003373;
  *((unsigned long *)&__m256i_result[3]) = 0xbbbbbbbbbbbbbbbb;
  *((unsigned long *)&__m256i_result[2]) = 0xbbbbbbbbbbbb8888;
  *((unsigned long *)&__m256i_result[1]) = 0xbbbbbbbbbbbbbbbb;
  *((unsigned long *)&__m256i_result[0]) = 0xbbbbbbbbbbbb8888;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x44);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[2]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[1]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[0]) = 0xf7f7f7f7f7f7f7f7;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xdededededededede;
  *((unsigned long *)&__m256i_result[2]) = 0xdededededededede;
  *((unsigned long *)&__m256i_result[1]) = 0xdededededededede;
  *((unsigned long *)&__m256i_result[0]) = 0xdededededededede;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x21);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x33);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x9090909090909090;
  *((unsigned long *)&__m256i_result[2]) = 0x9090909090909090;
  *((unsigned long *)&__m256i_result[1]) = 0x9090909090909090;
  *((unsigned long *)&__m256i_result[0]) = 0x9090909090909090;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x6f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0xf7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x5858585858585858;
  *((unsigned long *)&__m256i_result[2]) = 0x5858585858585858;
  *((unsigned long *)&__m256i_result[1]) = 0x5858585858585858;
  *((unsigned long *)&__m256i_result[0]) = 0x5858585858585858;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0xa7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x3d3d3d3d3d3d3d3d;
  *((unsigned long *)&__m256i_result[2]) = 0x3d3d3d3d3d3d3d3d;
  *((unsigned long *)&__m256i_result[1]) = 0x3d3d3d3d3d3d3d3d;
  *((unsigned long *)&__m256i_result[0]) = 0x3d3d3d3d3d3d3d3d;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0xc2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000010;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000010;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x9d9d9d9d9d9d9d8d;
  *((unsigned long *)&__m256i_result[2]) = 0x9d9d9d9d9d9d9d9d;
  *((unsigned long *)&__m256i_result[1]) = 0x9d9d9d9d9d9d9d8d;
  *((unsigned long *)&__m256i_result[0]) = 0x9d9d9d9d9d9d9d9d;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x62);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[2]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[1]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[0]) = 0x2a2a2a2a2a2a2a2a;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0xd5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000081220000812c;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000812000008120;
  *((unsigned long *)&__m256i_op0[1]) = 0x000081220000812c;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000812000008120;
  *((unsigned long *)&__m256i_result[3]) = 0xe9e968c9e9e968c1;
  *((unsigned long *)&__m256i_result[2]) = 0xe9e968c9e9e968c9;
  *((unsigned long *)&__m256i_result[1]) = 0xe9e968c9e9e968c1;
  *((unsigned long *)&__m256i_result[0]) = 0xe9e968c9e9e968c9;
  __m256i_out = __lasx_xvnori_b (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

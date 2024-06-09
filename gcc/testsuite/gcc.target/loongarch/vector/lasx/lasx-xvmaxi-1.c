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

  *((unsigned long *)&__m256i_op0[3]) = 0xffffd10000006459;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000441000000004;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000004;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000040400000104;
  *((unsigned long *)&__m256i_result[3]) = 0x0f0f0f0f0f0f6459;
  *((unsigned long *)&__m256i_result[2]) = 0x0f0f44100f0f0f0f;
  *((unsigned long *)&__m256i_result[1]) = 0x0f0f0f0f0f0f0f0f;
  *((unsigned long *)&__m256i_result[0]) = 0x0f0f0f0f0f0f0f0f;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8080808180808093;
  *((unsigned long *)&__m256i_op0[2]) = 0x80808081808080fe;
  *((unsigned long *)&__m256i_op0[1]) = 0x8080808180808093;
  *((unsigned long *)&__m256i_op0[0]) = 0x80808081808080fb;
  *((unsigned long *)&__m256i_result[3]) = 0xf5f5f5f5f5f5f5f5;
  *((unsigned long *)&__m256i_result[2]) = 0xf5f5f5f5f5f5f5fe;
  *((unsigned long *)&__m256i_result[1]) = 0xf5f5f5f5f5f5f5f5;
  *((unsigned long *)&__m256i_result[0]) = 0xf5f5f5f5f5f5f5fb;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[2]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[1]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[0]) = 0x0909090909090909;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, -4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[3]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[2]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[1]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[0]) = 0x0d0d0d0d0d0d0d0d;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0a0a0a0a7f0a0a0a;
  *((unsigned long *)&__m256i_result[2]) = 0x0a0a0a0a7f0a0a0a;
  *((unsigned long *)&__m256i_result[1]) = 0x0a0a0a0a7f0a0a0a;
  *((unsigned long *)&__m256i_result[0]) = 0x0a0a0a0a7f0a0a0a;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0707070707070707;
  *((unsigned long *)&__m256i_result[2]) = 0x0707070707070707;
  *((unsigned long *)&__m256i_result[1]) = 0x0707070707070707;
  *((unsigned long *)&__m256i_result[0]) = 0x0707070707070707;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[2]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[1]) = 0x0d0d0d0d0d0d0d0d;
  *((unsigned long *)&__m256i_result[0]) = 0x0d0d0d0d0d0d0d0d;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_op0[2]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_op0[1]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_op0[0]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[3]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[2]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[1]) = 0x2a2a2a2a2a2a2a2a;
  *((unsigned long *)&__m256i_result[0]) = 0x2a2a2a2a2a2a2a2a;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0c0c0c0c0c0c0c0c;
  *((unsigned long *)&__m256i_result[2]) = 0x0c0c0c0c0c0c0c0c;
  *((unsigned long *)&__m256i_result[1]) = 0x0c0c0c0c0c0c0c0c;
  *((unsigned long *)&__m256i_result[0]) = 0x0c0c0c0c0c0c0c0c;
  __m256i_out = __lasx_xvmaxi_b (__m256i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[2]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[1]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[0]) = 0x0005000500050005;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[0]) = 0x0001000100010001;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_op0[2]) = 0xffc00000ffc0ffc0;
  *((unsigned long *)&__m256i_op0[1]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_op0[0]) = 0xffc00000ffc0ffc0;
  *((unsigned long *)&__m256i_result[3]) = 0xfff9fff9fff9fff9;
  *((unsigned long *)&__m256i_result[2]) = 0xfff90000fff9fff9;
  *((unsigned long *)&__m256i_result[1]) = 0xfff9fff9fff9fff9;
  *((unsigned long *)&__m256i_result[0]) = 0xfff90000fff9fff9;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00ff00ff000c0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00ff00ff00040000;
  *((unsigned long *)&__m256i_result[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[2]) = 0x00ff00ff000c0000;
  *((unsigned long *)&__m256i_result[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[0]) = 0x00ff00ff00040000;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000000001ff;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffe0000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000000001ff;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffe0000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x00080008000801ff;
  *((unsigned long *)&__m256i_result[2]) = 0x0008000800080008;
  *((unsigned long *)&__m256i_result[1]) = 0x00080008000801ff;
  *((unsigned long *)&__m256i_result[0]) = 0x0008000800080008;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000c9;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000c9;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000000000c9;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000000000c9;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, -15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff8000ffa3;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000008000165a;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff8000ffa3;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000008000165a;
  *((unsigned long *)&__m256i_result[3]) = 0x0009000900090009;
  *((unsigned long *)&__m256i_result[2]) = 0x000900090009165a;
  *((unsigned long *)&__m256i_result[1]) = 0x0009000900090009;
  *((unsigned long *)&__m256i_result[0]) = 0x000900090009165a;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_op0[2]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_op0[1]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_op0[0]) = 0xfd12fd12fd12fd12;
  *((unsigned long *)&__m256i_result[3]) = 0x000a000a000a000a;
  *((unsigned long *)&__m256i_result[2]) = 0x000a000a000a000a;
  *((unsigned long *)&__m256i_result[1]) = 0x000a000a000a000a;
  *((unsigned long *)&__m256i_result[0]) = 0x000a000a000a000a;
  __m256i_out = __lasx_xvmaxi_h (__m256i_op0, 10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000401000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000401000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000401000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000401000000;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0110000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0110000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0110000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0110000000000080;
  *((unsigned long *)&__m256i_result[3]) = 0x0110000000000004;
  *((unsigned long *)&__m256i_result[2]) = 0x0110000000000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0110000000000004;
  *((unsigned long *)&__m256i_result[0]) = 0x0110000000000080;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000200000002;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000e0000000e;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000e0000000e;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000e0000000e;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000e0000000e;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffff040000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffff040000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff400000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff400000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000900000009;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[2]) = 0x000000081f20607a;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[0]) = 0x000000081f20607a;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, 8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffffffff;
  __m256i_out = __lasx_xvmaxi_w (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000ff80;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000ff80;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000ffff;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000009;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000ff1100;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000004560420;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000ff1100;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000004560420;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000ff1100;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000004560420;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000ff1100;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000004560420;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000007e1c7e1c;
  *((unsigned long *)&__m256i_op0[2]) = 0x7e00000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000007e1c7e1c;
  *((unsigned long *)&__m256i_op0[0]) = 0x7e00000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000007e1c7e1c;
  *((unsigned long *)&__m256i_result[2]) = 0x7e00000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x000000007e1c7e1c;
  *((unsigned long *)&__m256i_result[0]) = 0x7e00000000000000;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffff5;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff5;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffff5;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff5;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000007;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000007b007e;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000007b007e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000007b007e;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000007b007e;
  __m256i_out = __lasx_xvmaxi_d (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

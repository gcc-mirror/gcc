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

  *((unsigned long *)&__m256i_op0[3]) = 0xf96d674800000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x44a4330e2c7116c0;
  *((unsigned long *)&__m256i_op0[1]) = 0x14187a7822b653c0;
  *((unsigned long *)&__m256i_op0[0]) = 0xfbe0b866962b96d0;
  *((unsigned long *)&__m256i_result[3]) = 0xf90c0c0c00000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0ca40c0c0c0c0cc0;
  *((unsigned long *)&__m256i_result[1]) = 0x0c0c0c0c0cb60cc0;
  *((unsigned long *)&__m256i_result[0]) = 0xfbe0b80c960c96d0;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0010bfc80010bf52;
  *((unsigned long *)&__m256i_op0[2]) = 0xfff1bfca0011bfcb;
  *((unsigned long *)&__m256i_op0[1]) = 0x0010bfc80010bf52;
  *((unsigned long *)&__m256i_op0[0]) = 0xfff1bfca0011bfcb;
  *((unsigned long *)&__m256i_result[3]) = 0xf5f5bfc8f5f5bff5;
  *((unsigned long *)&__m256i_result[2]) = 0xf5f1bfcaf5f5bfcb;
  *((unsigned long *)&__m256i_result[1]) = 0xf5f5bfc8f5f5bff5;
  *((unsigned long *)&__m256i_result[0]) = 0xf5f1bfcaf5f5bfcb;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, -11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m256i_result[2]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m256i_result[1]) = 0xf8f8f8f8f8f8f8f8;
  *((unsigned long *)&__m256i_result[0]) = 0xf8f8f8f8f8f8f8f8;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, -8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000aaabffff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000aaabffff;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffff47b4ffff5878;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000b84b0000a787;
  *((unsigned long *)&__m256i_op0[1]) = 0xffff47b4ffff5878;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000b84b0000a787;
  *((unsigned long *)&__m256i_result[3]) = 0xffff07b4ffff0707;
  *((unsigned long *)&__m256i_result[2]) = 0x0000b8070000a787;
  *((unsigned long *)&__m256i_result[1]) = 0xffff07b4ffff0707;
  *((unsigned long *)&__m256i_result[0]) = 0x0000b8070000a787;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[2]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[1]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[0]) = 0xf7f7f7f7f7f7f7f7;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf3f3f3f3f3f3f3f3;
  *((unsigned long *)&__m256i_result[2]) = 0xf3f3f3f3f3f3f3f3;
  *((unsigned long *)&__m256i_result[1]) = 0xf3f3f3f3f3f3f3f3;
  *((unsigned long *)&__m256i_result[0]) = 0xf3f3f3f3f3f3f3f3;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[2]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[1]) = 0xf9f9f9f9f9f9f9f9;
  *((unsigned long *)&__m256i_result[0]) = 0xf9f9f9f9f9f9f9f9;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_op0[2]) = 0xc30e0000ff800000;
  *((unsigned long *)&__m256i_op0[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_op0[0]) = 0xc30e0000ff800000;
  *((unsigned long *)&__m256i_result[3]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_result[2]) = 0xc3030000ff800000;
  *((unsigned long *)&__m256i_result[1]) = 0xff800000ff800000;
  *((unsigned long *)&__m256i_result[0]) = 0xc3030000ff800000;
  __m256i_out = __lasx_xvmini_b (__m256i_op0, 3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff6fff6fff6fff6;
  *((unsigned long *)&__m256i_result[2]) = 0xfff6fff6fff6fff6;
  *((unsigned long *)&__m256i_result[1]) = 0xfff6fff6fff6fff6;
  *((unsigned long *)&__m256i_result[0]) = 0xfff6fff6fff6fff6;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, -10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1fffffff1fffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0383634303836343;
  *((unsigned long *)&__m256i_op0[1]) = 0x1fffffff1fffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0383634303836343;
  *((unsigned long *)&__m256i_result[3]) = 0x0002ffff0002ffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[1]) = 0x0002ffff0002ffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0002000200020002;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, 2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000f7bc0001f7bd;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000f93b0000017c;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000f7bc0001f7bd;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000f93b0000017b;
  *((unsigned long *)&__m256i_result[3]) = 0xfff2f7bcfff2f7bd;
  *((unsigned long *)&__m256i_result[2]) = 0xfff2f93bfff2fff2;
  *((unsigned long *)&__m256i_result[1]) = 0xfff2f7bcfff2f7bd;
  *((unsigned long *)&__m256i_result[0]) = 0xfff2f93bfff2fff2;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, 6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, 13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfff9fff9fff9fff9;
  *((unsigned long *)&__m256i_result[2]) = 0xfff9fff9fff9fff9;
  *((unsigned long *)&__m256i_result[1]) = 0xfff9fff9fff9fff9;
  *((unsigned long *)&__m256i_result[0]) = 0xfff9fff9fff9fff9;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, -7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff3fff3fff3fff3;
  *((unsigned long *)&__m256i_result[2]) = 0xfff3fff3fff3fff3;
  *((unsigned long *)&__m256i_result[1]) = 0xfff3fff3fff3fff3;
  *((unsigned long *)&__m256i_result[0]) = 0xfff3fff3fff3fff3;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, -13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff2fff2fff2fff2;
  *((unsigned long *)&__m256i_result[2]) = 0xfff2fff2fff2fff2;
  *((unsigned long *)&__m256i_result[1]) = 0xfff2fff2fff2fff2;
  *((unsigned long *)&__m256i_result[0]) = 0xfff2fff2fff2fff2;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_h (__m256i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[2]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[1]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_op0[0]) = 0x555555ab555555ab;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000400000004;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000400000004;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000400000004;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000400000004;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffff8c80;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000fff0e400;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffff8c80;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000fff0e400;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000100000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff2fffffff2;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff2fffffff2;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff2fffffff2;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff2fffffff2;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, -14);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0001000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000100010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0001000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000100010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000a00000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000000010000000a;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000a00000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000010000000a;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, 10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff8fffffff8;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff8fffffff8;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff8fffffff8;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff8fffffff8;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, -8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff7fffffff7;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff7fffffff7;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, -9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, 4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffff0fffffff0;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, -16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmini_w (__m256i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, 11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x327f010101010102;
  *((unsigned long *)&__m256i_op0[2]) = 0x6300000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x327f010101010102;
  *((unsigned long *)&__m256i_op0[0]) = 0x6300000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffff4;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff4;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, -12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000009;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, 9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, 13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, -1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_op0[2]) = 0xff00ff007f007f00;
  *((unsigned long *)&__m256i_op0[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00ff007f007f00;
  *((unsigned long *)&__m256i_result[3]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_result[2]) = 0xff00ff007f007f00;
  *((unsigned long *)&__m256i_result[1]) = 0xff00ff00ff00ff00;
  *((unsigned long *)&__m256i_result[0]) = 0xff00ff007f007f00;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, -5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000000c;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000000c;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvmini_d (__m256i_op0, 12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

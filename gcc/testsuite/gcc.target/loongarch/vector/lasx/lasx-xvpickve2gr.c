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

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0cc08723ff900001;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xcc9b89f2f6cef440;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x7);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000ff80;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  int_result = 0x000000000000ffff;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x6);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff00000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ff90ff81;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000007f;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ff90ff81;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000007f;
  int_result = 0x000000000000007f;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x4);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffefdfffffefd;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000fffffefd;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x4);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x555555553f800000;
  *((unsigned long *)&__m256i_op0[2]) = 0x5555555580000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x555555553f800000;
  *((unsigned long *)&__m256i_op0[0]) = 0x5555555580000000;
  int_result = 0x0000000055555555;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x5);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  long_int_result = 0x0000000000000000;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0x0002000400000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0002000200020006;
  unsigned_int_result = 0x0000000000020006;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x1f0fdf7f3e3b31d4;
  *((unsigned long *)&__m256i_op0[2]) = 0x7ff8000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x1f0fdf7f3e3b31d4;
  *((unsigned long *)&__m256i_op0[0]) = 0x7ff8000000000000;
  long_int_result = 0x1f0fdf7f3e3b31d4;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00fe01fc01fe01fc;
  *((unsigned long *)&__m256i_op0[2]) = 0x012c002c001c0006;
  *((unsigned long *)&__m256i_op0[1]) = 0x00fe01fc01fe0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x012c002c001c000a;
  long_int_result = 0xfe01fc01fe0000;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  unsigned_long_int_result = 0x00000000ffffffff;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_long_int_out, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x5);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffff0100;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  long_int_result = 0x00000000ffff0100;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7ff0000000000000;
  int_result = 0x000000007ff00000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  unsigned_long_int_result = 0x00000000ffffffff;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000000ff;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x6);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0xffffffffffffffff;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x5);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffff0100000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffff0100000001;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x7);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffff0008;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffff0008;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x6);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  long_int_result = 0x0000000000000000;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  long_int_result = 0x000000000000ffff;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_op0[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000100010;
  *((unsigned long *)&__m256i_op0[0]) = 0x0010001000100010;
  unsigned_int_result = 0x0000000000100010;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_int_result = 0x0000000000000000;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000100040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000100040;
  unsigned_int_result = 0x0000000000000040;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x6);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x6);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  long_int_result = 0xffffffffffffffff;
  long_int_out = __lasx_xvpickve2gr_d (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, long_int_result, long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  unsigned_int_result = 0x00000000ffffffff;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x5);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  unsigned_int_result = 0x00000000ffffffff;
  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x4);
  ASSERTEQ_int (__LINE__, unsigned_int_result, unsigned_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x1);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  unsigned_long_int_result = 0x0000000000000000;
  unsigned_long_int_out = __lasx_xvpickve2gr_du (__m256i_op0, 0x3);
  ASSERTEQ_int (__LINE__, unsigned_long_int_result, unsigned_long_int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x0);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  int_result = 0x00000000ffffffff;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffd880;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffd880;
  int_result = 0x0000000000000000;
  int_out = __lasx_xvpickve2gr_w (__m256i_op0, 0x2);
  ASSERTEQ_int (__LINE__, int_result, int_out);

  return 0;
}

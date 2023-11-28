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

  *((unsigned long *)&__m128i_op0[1]) = 0x0027002a00030018;
  *((unsigned long *)&__m128i_op0[0]) = 0x7f4300177f7a7f59;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0027002a00080018;
  *((unsigned long *)&__m128i_result[0]) = 0x7f4300177f7a7f59;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000007f00000004;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000401000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0001000100000004;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000110000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000007f00000004;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000800000000;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x75b043c4d17db125;
  *((unsigned long *)&__m128i_op0[0]) = 0xeef8227b4f8017b1;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x027c027c000027c0;
  *((unsigned long *)&__m128i_result[1]) = 0x75b043c4007db125;
  *((unsigned long *)&__m128i_result[0]) = 0xeef8227b4f8017b1;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0xb);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000ff000000ff00;
  *((unsigned long *)&__m128i_op1[1]) = 0x03c0000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x03c0038000000380;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000ff000000ff00;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x0);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000010a000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ffff0000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ffff000000ff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000010a000b;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000009;
  *((unsigned long *)&__m128i_op0[0]) = 0x5b35342c979955da;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000009;
  *((unsigned long *)&__m128i_result[0]) = 0x5b35342c970455da;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0010000000000000;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0008000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00d3012b015700bb;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001002affca0070;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001ca02f854;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100013fa0;
  *((unsigned long *)&__m128i_result[1]) = 0x00d3012b015700bb;
  *((unsigned long *)&__m128i_result[0]) = 0x00010000ffca0070;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000fffe0001;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000000000bf;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000000002bb;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x00080000fffe0001;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000545cffffab1d;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff81a800003bea;
  *((unsigned long *)&__m128i_op1[1]) = 0x13f9c5b60028a415;
  *((unsigned long *)&__m128i_op1[0]) = 0x545cab1d81a83bea;
  *((unsigned long *)&__m128i_result[1]) = 0x0000545cffff0001;
  *((unsigned long *)&__m128i_result[0]) = 0xffff81a800003bea;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0008000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x7);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000001b;
  *((unsigned long *)&__m128i_result[0]) = 0x000000000000001b;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0008000000000000;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x3);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x379674c000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff7ffffffffe;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000fffffffe;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x379674c000000000;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x8);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000800000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x6);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x001a001a001a000b;
  *((unsigned long *)&__m128i_op0[0]) = 0x001a001a001a000b;
  *((unsigned long *)&__m128i_op1[1]) = 0x001a001a001a000b;
  *((unsigned long *)&__m128i_op1[0]) = 0x001a001a001a000b;
  *((unsigned long *)&__m128i_result[1]) = 0x001a001a001a0008;
  *((unsigned long *)&__m128i_result[0]) = 0x001a001a001a000b;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x4);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x02f3030303030303;
  *((unsigned long *)&__m128i_op1[1]) = 0x004d004d004d004d;
  *((unsigned long *)&__m128i_op1[0]) = 0x004d004d004d004d;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x02f3030303100303;
  __m128i_out = __lsx_vfrstpi_b (__m128i_op0, __m128i_op1, 0x2);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_op0[0]) = 0x00007770ffff941d;
  *((unsigned long *)&__m128i_op1[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_op1[0]) = 0x00007770ffff941d;
  *((unsigned long *)&__m128i_result[1]) = 0x000000400000004c;
  *((unsigned long *)&__m128i_result[0]) = 0x00007770ffff941d;
  __m128i_out = __lsx_vfrstpi_h (__m128i_op0, __m128i_op1, 0x5);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

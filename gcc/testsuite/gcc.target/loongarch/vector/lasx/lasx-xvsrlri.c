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
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x33);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x8000000080000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x8000000080000000;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffff000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffff000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffff000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffff000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000001000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000001000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x28);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000505;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffff0002fffefffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff0002ff7e8286;
  *((unsigned long *)&__m256i_op0[1]) = 0xffff0002fffefffe;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff0002ffff0001;
  *((unsigned long *)&__m256i_result[3]) = 0x0202000002020202;
  *((unsigned long *)&__m256i_result[2]) = 0x0202000002010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0202000002020202;
  *((unsigned long *)&__m256i_result[0]) = 0x0202000002020000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001000000010;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xe1616161e1614e60;
  *((unsigned long *)&__m256i_op0[2]) = 0xe1616161e1614e60;
  *((unsigned long *)&__m256i_op0[1]) = 0xe1616161e1614e60;
  *((unsigned long *)&__m256i_op0[0]) = 0xe1616161e1614e60;
  *((unsigned long *)&__m256i_result[3]) = 0x0703030307030203;
  *((unsigned long *)&__m256i_result[2]) = 0x0703030307030203;
  *((unsigned long *)&__m256i_result[1]) = 0x0703030307030203;
  *((unsigned long *)&__m256i_result[0]) = 0x0703030307030203;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x5);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00003f3fc6c68787;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00003f3f87870000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00003f3fc6c68787;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00003f3f87870000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0101010183f95466;
  *((unsigned long *)&__m256i_op0[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[1]) = 0x01010101d58efe94;
  *((unsigned long *)&__m256i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[3]) = 0x0000101000083f95;
  *((unsigned long *)&__m256i_result[2]) = 0x0000101000001010;
  *((unsigned long *)&__m256i_result[1]) = 0x00001010000d58f0;
  *((unsigned long *)&__m256i_result[0]) = 0x0000101000001010;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x23);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0010002000100020;
  *((unsigned long *)&__m256i_result[2]) = 0x0010002000100020;
  *((unsigned long *)&__m256i_result[1]) = 0x0010002000100020;
  *((unsigned long *)&__m256i_result[0]) = 0x0010002000100020;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff0000ffff0001;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff0000ffff0001;
  *((unsigned long *)&__m256i_result[3]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_result[2]) = 0x0020000000200000;
  *((unsigned long *)&__m256i_result[1]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_result[0]) = 0x0020000000200000;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000040000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000040000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000020000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000020000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000e000e000e000e;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000e000e000e000e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x39);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000040000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000040000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000040000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000040000000000;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x04e8296f18181818;
  *((unsigned long *)&__m256i_op0[2]) = 0x132feea900000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x04e8296f18181818;
  *((unsigned long *)&__m256i_op0[0]) = 0x132feea900000000;
  *((unsigned long *)&__m256i_result[3]) = 0x04e8296f18181818;
  *((unsigned long *)&__m256i_result[2]) = 0x132feea900000000;
  *((unsigned long *)&__m256i_result[1]) = 0x04e8296f18181818;
  *((unsigned long *)&__m256i_result[0]) = 0x132feea900000000;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000038000000268;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000038000000268;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff7fff7fff;
  *((unsigned long *)&__m256i_result[3]) = 0x000001200000011a;
  *((unsigned long *)&__m256i_result[2]) = 0x2040204020402040;
  *((unsigned long *)&__m256i_result[1]) = 0x000001200000011a;
  *((unsigned long *)&__m256i_result[0]) = 0x2040204020402040;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xff81001dff9dff9e;
  *((unsigned long *)&__m256i_op0[2]) = 0xff81001dff9d003b;
  *((unsigned long *)&__m256i_op0[1]) = 0xff81001dff9dff9e;
  *((unsigned long *)&__m256i_op0[0]) = 0xff81001dff9d003b;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001000000010;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_w (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000fffa003e;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000fffb009c;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000fffa003e;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000fffb009c;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000002;
  __m256i_out = __lasx_xvsrlri_d (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffe00000001;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffe00000001;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0020004000400040;
  *((unsigned long *)&__m256i_result[2]) = 0x0020004000400040;
  *((unsigned long *)&__m256i_result[1]) = 0x0020004000400040;
  *((unsigned long *)&__m256i_result[0]) = 0x0020004000400040;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000ff0000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000800000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000800000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000800000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000800000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffffffbfffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000800000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffffffbfffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000800000;
  *((unsigned long *)&__m256i_result[3]) = 0x0102020202010202;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000010000;
  *((unsigned long *)&__m256i_result[1]) = 0x0102020202010202;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000010000;
  __m256i_out = __lasx_xvsrlri_b (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000006;
  *((unsigned long *)&__m256i_op0[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000006;
  *((unsigned long *)&__m256i_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0008000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0008000000000000;
  __m256i_out = __lasx_xvsrlri_h (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

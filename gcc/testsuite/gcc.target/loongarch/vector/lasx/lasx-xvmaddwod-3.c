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
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_op2[2]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_op2[1]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_op2[0]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_result[3]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[2]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[1]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[0]) = 0xff01ff01ff01ff01;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000200000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000003;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000200000000;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op0[2]) = 0x000b8f81b8c850f4;
  *((unsigned long *)&__m256i_op0[1]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op0[0]) = 0x000b8f81b8c850f4;
  *((unsigned long *)&__m256i_op1[3]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op1[2]) = 0xd0d8eecf383fdf0d;
  *((unsigned long *)&__m256i_op1[1]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op1[0]) = 0xd0d8eecf383fdf0d;
  *((unsigned long *)&__m256i_op2[3]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op2[2]) = 0x000b8f81b8c850f4;
  *((unsigned long *)&__m256i_op2[1]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op2[0]) = 0x000b8f81b8c850f4;
  *((unsigned long *)&__m256i_result[3]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_result[2]) = 0x000b2673a90896a4;
  *((unsigned long *)&__m256i_result[1]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_result[0]) = 0x000b2673a90896a4;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc03ae000ffff6000;
  *((unsigned long *)&__m256i_op0[2]) = 0xc600000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xc03ae000ffff6000;
  *((unsigned long *)&__m256i_op0[0]) = 0xc600000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffc03fffffffc0;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffc00000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffc03fffffffc0;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffc00000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xc03ae000ffff6000;
  *((unsigned long *)&__m256i_result[2]) = 0xc600000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xc03ae000ffff6000;
  *((unsigned long *)&__m256i_result[0]) = 0xc600000000000000;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff7fff000003c0;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff7fff000003c0;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000fc300000fc40;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000fc300000fc40;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fff7c030000ffc4;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fff7c030000ffc4;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00f7000000f70006;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x00f7000000f70006;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffff0007a861;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffff0007a861;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xbff0000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xbff0000000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000001fffffffe;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffff0002fffeffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffff0002fffeffff;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000505;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000001000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000001000;
  *((unsigned long *)&__m256i_op2[3]) = 0xf000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0xf000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0xf000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0xf000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000627;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000627;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x1f60000000c00000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x1f60000000c00000;
  *((unsigned long *)&__m256i_op2[3]) = 0x7fff7fff05407fff;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x7fff7fff05407fff;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000627;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000627;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffffffff;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x437f201f201f2020;
  *((unsigned long *)&__m256i_op1[2]) = 0x037f201f001f2020;
  *((unsigned long *)&__m256i_op1[1]) = 0x437f201f201f2020;
  *((unsigned long *)&__m256i_op1[0]) = 0x037f201f001f2020;
  *((unsigned long *)&__m256i_op2[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x21bb481000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x01bf481000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x21bb481000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x01bf481000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x000000010000685e;
  *((unsigned long *)&__m256i_op2[2]) = 0x000020a4ffffbe4f;
  *((unsigned long *)&__m256i_op2[1]) = 0x000000010000685e;
  *((unsigned long *)&__m256i_op2[0]) = 0x000020a4ffffbe4f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x001fffffffe00000;
  *((unsigned long *)&__m256i_op1[2]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x001fffffffe00000;
  *((unsigned long *)&__m256i_op1[0]) = 0x7fffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x000000ffffff1dff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffff1dffffff1dff;
  *((unsigned long *)&__m256i_op2[1]) = 0x000000ffffff1dff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffff1dffffff1dff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffff0020;
  *((unsigned long *)&__m256i_result[2]) = 0xffff8001ffff0001;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffff0020;
  *((unsigned long *)&__m256i_result[0]) = 0xffff8001ffff0001;
  __m256i_out
      = __lasx_xvmaddwod_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffff8c80;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffffffe40;
  *((unsigned long *)&__m256i_op1[3]) = 0x00f9f90079f9f9f9;
  *((unsigned long *)&__m256i_op1[2]) = 0x79f9f9f900000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x00f9f90079f9f9f9;
  *((unsigned long *)&__m256i_op1[0]) = 0x79f9f9f900000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffff8c80;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffe40;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000154dc84;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000089;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x00000000000000ff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000154dc84;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000089;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffdc;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffdc;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffdc;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffdc;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffeffffffdd;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffdc;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op1[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op1[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op1[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op1[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[0]) = 0x0101010101010101;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000fffcfffcfffc;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000fffcfffcfffc;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x00000000f9f9f9f9;
  *((unsigned long *)&__m256i_op2[2]) = 0x00000000faf3f3f2;
  *((unsigned long *)&__m256i_op2[1]) = 0x00000000f9f9f9f9;
  *((unsigned long *)&__m256i_op2[0]) = 0x00000000faf3f3f2;
  *((unsigned long *)&__m256i_result[3]) = 0x0000fffcfffcfffc;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000fffcfffcfffc;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfbff0000ffff0000;
  *((unsigned long *)&__m256i_op0[2]) = 0xff00000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfbff0000ffff0000;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfbff0000ffff0000;
  *((unsigned long *)&__m256i_result[2]) = 0xff00000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfbff0000ffff0000;
  *((unsigned long *)&__m256i_result[0]) = 0xff00000000000000;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x000000000000000c;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x000000000000000c;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0101010101010110;
  *((unsigned long *)&__m256i_op2[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op2[1]) = 0x0101010101010110;
  *((unsigned long *)&__m256i_op2[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwod_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000eef14fe8;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0202020201010000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00000000eef14fe8;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0202020201010000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0xfe02fe02fee5fe22;
  *((unsigned long *)&__m256i_op2[0]) = 0xff49fe4200000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000eef14fe8;
  *((unsigned long *)&__m256i_result[1]) = 0xfffe928f1313c9cc;
  *((unsigned long *)&__m256i_result[0]) = 0x4244020201010000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op0[2]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op0[1]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op0[0]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[2]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[1]) = 0x0005000500050005;
  *((unsigned long *)&__m256i_result[0]) = 0x0005000500050005;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00000000fffffff6;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x00000000fffffff6;
  *((unsigned long *)&__m256i_op2[3]) = 0x3f3f3f3f3f3f3f3f;
  *((unsigned long *)&__m256i_op2[2]) = 0x3f3f3f3f3f3f3f3f;
  *((unsigned long *)&__m256i_op2[1]) = 0x000000003f3f3f3f;
  *((unsigned long *)&__m256i_op2[0]) = 0x3f3f3f3f00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_result[2]) = 0xc6c6c6c68787878a;
  *((unsigned long *)&__m256i_result[1]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_result[0]) = 0x8787878a00000000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000fffffff6;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000fffffff6;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op2[3]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_op2[2]) = 0xc6c6c6c68787878a;
  *((unsigned long *)&__m256i_op2[1]) = 0x000000003f3f3f3c;
  *((unsigned long *)&__m256i_op2[0]) = 0x8787878a00000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffe3;
  *((unsigned long *)&__m256i_result[2]) = 0x63636344c3c3c4f6;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffc3;
  *((unsigned long *)&__m256i_result[0]) = 0xc3c3c500fffffff6;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000900000009;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000009;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000009;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0004000f00100003;
  *((unsigned long *)&__m256i_op1[2]) = 0x000400030010000f;
  *((unsigned long *)&__m256i_op1[1]) = 0x0004000f00100003;
  *((unsigned long *)&__m256i_op1[0]) = 0x000400030010000f;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xfffbfffcffeffff0;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xfffbfffcffeffff0;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000b0cfffff4f3;
  *((unsigned long *)&__m256i_op0[2]) = 0x000f9bb562f56c80;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000b0cfffff4f3;
  *((unsigned long *)&__m256i_op0[0]) = 0x000f9bb562f56c80;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op1[2]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op1[0]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op2[2]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000ffff0002fffd;
  *((unsigned long *)&__m256i_op2[0]) = 0x04f104f104f504ed;
  *((unsigned long *)&__m256i_result[3]) = 0x0018761ed60b5d7f;
  *((unsigned long *)&__m256i_result[2]) = 0xabdcdc9938afafe9;
  *((unsigned long *)&__m256i_result[1]) = 0x0018761ed60b5d7f;
  *((unsigned long *)&__m256i_result[0]) = 0xabdcdc9938afafe9;
  __m256i_out
      = __lasx_xvmaddwev_q_du_d (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

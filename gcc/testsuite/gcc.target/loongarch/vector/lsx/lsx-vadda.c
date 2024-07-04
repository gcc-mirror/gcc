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

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x52527d7d52527d7d;
  *((unsigned long *)&__m128i_op1[0]) = 0x52527d7d52527d7d;
  *((unsigned long *)&__m128i_result[1]) = 0x52527d7d52527d7d;
  *((unsigned long *)&__m128i_result[0]) = 0x52527d7d52527d7d;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000001fffc001f;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010202050120;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010102020202;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0003000300030003;
  *((unsigned long *)&__m128i_op0[0]) = 0x0003000700020005;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0003000300030003;
  *((unsigned long *)&__m128i_result[0]) = 0x0003000700020005;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010101010101;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x4f8000004f800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x4f8000004f800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4f8000004f800000;
  *((unsigned long *)&__m128i_result[0]) = 0x4f8000004f800000;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffcfffcfffcfffc;
  *((unsigned long *)&__m128i_result[1]) = 0x0003000300030004;
  *((unsigned long *)&__m128i_result[0]) = 0x0003000300030004;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5c9c9c9ce3636363;
  *((unsigned long *)&__m128i_op0[0]) = 0x63635c9e63692363;
  *((unsigned long *)&__m128i_op1[1]) = 0xf0fd800080000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000a00028004000;
  *((unsigned long *)&__m128i_result[1]) = 0x6b9fe3649c9d6363;
  *((unsigned long *)&__m128i_result[0]) = 0x6363bc9e8b696363;
  __m128i_out = __lsx_vadda_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_op0[0]) = 0x1111111111111111;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000002000000020;
  *((unsigned long *)&__m128i_result[1]) = 0x1111113111111131;
  *((unsigned long *)&__m128i_result[0]) = 0x1111113111111131;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000006a9a5c;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000092444;
  *((unsigned long *)&__m128i_op1[1]) = 0x00000000006a9a5c;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000092444;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000d4ccb8;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000124888;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000001;
  *((unsigned long *)&__m128i_op0[0]) = 0x76f424887fffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff082f000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x003f000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x000f7d1000000001;
  *((unsigned long *)&__m128i_result[0]) = 0x773324887fffffff;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xfffffacdb6dbecac;
  *((unsigned long *)&__m128i_op0[0]) = 0x1f5533a694f902c0;
  *((unsigned long *)&__m128i_op1[1]) = 0x5a6f5c53ebed3faa;
  *((unsigned long *)&__m128i_op1[0]) = 0xa36aca4435b8b8e1;
  *((unsigned long *)&__m128i_result[1]) = 0x5a6f61865d36d3aa;
  *((unsigned long *)&__m128i_result[0]) = 0x7bea6962a0bfb621;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000008140c80;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000008140c80;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000fffe0000ff45;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff000000b9;
  *((unsigned long *)&__m128i_op1[1]) = 0xffd5002affffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x343d8dc6b0ed5a08;
  *((unsigned long *)&__m128i_result[1]) = 0x012b012c01010246;
  *((unsigned long *)&__m128i_result[0]) = 0x353e743b50135a4f;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0003c853c843c87e;
  *((unsigned long *)&__m128i_op1[0]) = 0x0003c853c843c87e;
  *((unsigned long *)&__m128i_result[1]) = 0x0003c853c843c87e;
  *((unsigned long *)&__m128i_result[0]) = 0x0003c853c843c87e;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000200000002000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffe000ffdf;
  *((unsigned long *)&__m128i_result[1]) = 0x0000200000002001;
  *((unsigned long *)&__m128i_result[0]) = 0x000000001fff0021;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0101010101010109;
  *((unsigned long *)&__m128i_result[0]) = 0x0101010101010101;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000005452505;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000004442403e4;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffffffe0;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000005452505;
  *((unsigned long *)&__m128i_result[0]) = 0x000000044525043c;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x5d7f5d807fea807f;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x5d7f5d807fea807f;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0xbafebb00ffd500fe;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_d (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000208000002080;
  *((unsigned long *)&__m128i_result[0]) = 0x0000208000002080;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x8000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x8000000000000000;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_op0[0]) = 0x7fff00007fff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0x003f0000003f0000;
  *((unsigned long *)&__m128i_op1[0]) = 0x003f0000003f0000;
  *((unsigned long *)&__m128i_result[1]) = 0x803e0000803e0000;
  *((unsigned long *)&__m128i_result[0]) = 0x803e0000803e0000;
  __m128i_out = __lsx_vadda_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000800000008000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000800000008000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000800000008000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000800000008000;
  __m128i_out = __lsx_vadda_b (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000001400000014;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000001400000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xfff9000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffc000400000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0007001400000014;
  *((unsigned long *)&__m128i_result[0]) = 0x0004001000000000;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op0[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op1[1]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_op1[0]) = 0x8080808080808080;
  *((unsigned long *)&__m128i_result[1]) = 0xfefeff00fefeff00;
  *((unsigned long *)&__m128i_result[0]) = 0xfefeff00fefeff00;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vadda_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000024170000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000020300000;
  *((unsigned long *)&__m128i_result[1]) = 0x00000000084d12ce;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000044470000;
  __m128i_out = __lsx_vadda_h (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[1]) = 0xff01ff01ac025c87;
  *((unsigned long *)&__m128i_op1[0]) = 0xff01ff01ac465ca1;
  *((unsigned long *)&__m128i_result[1]) = 0x64616462b76106dc;
  *((unsigned long *)&__m128i_result[0]) = 0x64616462b71d06c2;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffaeffaeffaeffae;
  *((unsigned long *)&__m128i_op1[0]) = 0xffaeffaeffaeffae;
  *((unsigned long *)&__m128i_result[1]) = 0x0051005200510052;
  *((unsigned long *)&__m128i_result[0]) = 0x0051005200510052;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x3a8000003a800000;
  *((unsigned long *)&__m128i_op0[0]) = 0x3bc000003a800000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x0a0000000a000000;
  *((unsigned long *)&__m128i_result[1]) = 0x4480000044800000;
  *((unsigned long *)&__m128i_result[0]) = 0x45c0000044800000;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_result[1]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[0]) = 0x6363636463636363;
  __m128i_out = __lsx_vadda_w (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

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

  *((unsigned long *)&__m128i_op0[1]) = 0x16161616a16316b0;
  *((unsigned long *)&__m128i_op0[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_op1[1]) = 0x16161616a16316b0;
  *((unsigned long *)&__m128i_op1[0]) = 0x6363636363636363;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000001fffffffe;
  *((unsigned long *)&__m128i_op1[1]) = 0x5a5a5a5a5b5a5b5a;
  *((unsigned long *)&__m128i_op1[0]) = 0x5a5a5a5a5b5a5b5a;
  *((unsigned long *)&__m128i_result[1]) = 0x00000001494b494a;
  *((unsigned long *)&__m128i_result[0]) = 0x00000001494b494a;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xd70b30c96ea9f4e8;
  *((unsigned long *)&__m128i_op1[0]) = 0xa352bfac9269e0aa;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffeb;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_bu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x7f801fa06451ef11;
  *((unsigned long *)&__m128i_op1[0]) = 0x68bcf93435ed25ed;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000022666621;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffdd9999da;
  *((unsigned long *)&__m128i_op1[1]) = 0x7f7f7f7f00107f04;
  *((unsigned long *)&__m128i_op1[0]) = 0x7f0000fd7f0000fd;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000066621;
  *((unsigned long *)&__m128i_result[0]) = 0x01ff00085e9900ab;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x000000000000bd3d;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000007fff0000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xefffdffff0009d3d;
  *((unsigned long *)&__m128i_result[1]) = 0x000000000000bd3d;
  *((unsigned long *)&__m128i_result[0]) = 0x000000007fff0000;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000004870ba0;
  *((unsigned long *)&__m128i_op1[1]) = 0x478b478b38031779;
  *((unsigned long *)&__m128i_op1[0]) = 0x6b769e690fa1e119;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000004870ba0;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_op0[0]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_op1[1]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_op1[0]) = 0x2006454690d3de87;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_op0[0]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_op1[1]) = 0x02b010f881a281a2;
  *((unsigned long *)&__m128i_op1[0]) = 0x27b169bbb8145f50;
  *((unsigned long *)&__m128i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m128i_result[0]) = 0x0002000200020002;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffff100000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_op1[0]) = 0x0010001000100010;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000f000000000000;
  __m128i_out = __lsx_vmod_hu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000100000001;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000fffe0000fffe;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffff00ffffff00;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffff00ffffff00;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000fffe0000fffe;
  __m128i_out = __lsx_vmod_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_op1[0]) = 0x8000000080000000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_wu (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_op1[0]) = 0x0000000000000200;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m128i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000000ffff0000;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x0000000000000000;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_op0[0]) = 0x0001000101fd01fe;
  *((unsigned long *)&__m128i_op1[1]) = 0xff80ff80ff80ff80;
  *((unsigned long *)&__m128i_op1[0]) = 0xff80ff8080008000;
  *((unsigned long *)&__m128i_result[1]) = 0x0001000100010001;
  *((unsigned long *)&__m128i_result[0]) = 0x0001000101fd01fe;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffcafff8ff;
  *((unsigned long *)&__m128i_op0[0]) = 0x00000000000000a0;
  *((unsigned long *)&__m128i_op1[1]) = 0xff2cfed4fea8ff44;
  *((unsigned long *)&__m128i_op1[0]) = 0xfffeffff0035ff8f;
  *((unsigned long *)&__m128i_result[1]) = 0x00d3012acc56f9bb;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000000000a0;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x37c0001000000000;
  *((unsigned long *)&__m128i_op1[1]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_op1[0]) = 0x0004000400040004;
  *((unsigned long *)&__m128i_result[1]) = 0x0003c853c843c844;
  *((unsigned long *)&__m128i_result[0]) = 0x0003c853c843c844;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_op0[0]) = 0x000000003ddc5dac;
  *((unsigned long *)&__m128i_op1[1]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_op1[0]) = 0xfcfcfcdcfcfcfcdc;
  *((unsigned long *)&__m128i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m128i_result[0]) = 0x000000003ddc5dac;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  *((unsigned long *)&__m128i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m128i_op0[0]) = 0xfffffffefffff784;
  *((unsigned long *)&__m128i_op1[1]) = 0x10f8000100000001;
  *((unsigned long *)&__m128i_op1[0]) = 0x00000001000010f8;
  *((unsigned long *)&__m128i_result[1]) = 0x0177fff0fffffff0;
  *((unsigned long *)&__m128i_result[0]) = 0x00000000011ff8bc;
  __m128i_out = __lsx_vmod_du (__m128i_op0, __m128i_op1);
  ASSERTEQ_64 (__LINE__, __m128i_result, __m128i_out);

  return 0;
}

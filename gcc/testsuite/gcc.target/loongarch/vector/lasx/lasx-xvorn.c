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
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffff0000ffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xbf28b0686066be60;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[0]) = 0x40d74f979f99419f;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffefffffefc;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x01480000052801a2;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x00000000ffdcff64;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffefd;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0006000000040000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0002555500000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0006000000040000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0002555500000000;
  *((unsigned long *)&__m256i_result[3]) = 0xfff9fffffffbffff;
  *((unsigned long *)&__m256i_result[2]) = 0xfffdaaaaffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xfff9fffffffbffff;
  *((unsigned long *)&__m256i_result[0]) = 0xfffdaaaaffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000022;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000022;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000236200005111;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000175e0000490d;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000236200005111;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000175e0000490d;
  *((unsigned long *)&__m256i_op1[3]) = 0x0002000000020000;
  *((unsigned long *)&__m256i_op1[2]) = 0x00220021004a007e;
  *((unsigned long *)&__m256i_op1[1]) = 0x0002000000020000;
  *((unsigned long *)&__m256i_op1[0]) = 0x00220021004a007e;
  *((unsigned long *)&__m256i_result[3]) = 0xfffdfffffffdffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffddffdeffb5ff8d;
  *((unsigned long *)&__m256i_result[1]) = 0xfffdfffffffdffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffddffdeffb5ff8d;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_op1[3]) = 0x00ff010000ff017e;
  *((unsigned long *)&__m256i_op1[2]) = 0x01fe01ae00ff00ff;
  *((unsigned long *)&__m256i_op1[1]) = 0x00ff010000ff017e;
  *((unsigned long *)&__m256i_op1[0]) = 0x01fe01ae00ff00ff;
  *((unsigned long *)&__m256i_result[3]) = 0xff00feffff00fe81;
  *((unsigned long *)&__m256i_result[2]) = 0xfe01fe51ff00ff40;
  *((unsigned long *)&__m256i_result[1]) = 0xff00feffff00fe81;
  *((unsigned long *)&__m256i_result[0]) = 0xfe01fe51ff00ff40;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffe0df9f8e;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffe0df9f8e;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xe07de0801f20607a;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffe0df9f8f;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffe0df9f8f;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[2]) = 0x800000ff800000ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op0[0]) = 0x800000ff800000ff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x8000800080008000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffff7fffffff7fff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffff7fffffff7fff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0xdf80df80df80dfff;
  *((unsigned long *)&__m256i_op1[2]) = 0x8080808080808080;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffdf80dfff;
  *((unsigned long *)&__m256i_op1[0]) = 0x8080808080808080;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x498100814843ffe1;
  *((unsigned long *)&__m256i_op0[2]) = 0x4981008168410001;
  *((unsigned long *)&__m256i_op0[1]) = 0x498100814843ffe1;
  *((unsigned long *)&__m256i_op0[0]) = 0x4981008168410001;
  *((unsigned long *)&__m256i_op1[3]) = 0x40f69fe73c26f4ee;
  *((unsigned long *)&__m256i_op1[2]) = 0x7ff8000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x40f69fe73c26f4ee;
  *((unsigned long *)&__m256i_op1[0]) = 0x7ff8000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xff896099cbdbfff1;
  *((unsigned long *)&__m256i_result[2]) = 0xc987ffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xff896099cbdbfff1;
  *((unsigned long *)&__m256i_result[0]) = 0xc987ffffffffffff;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xfffffffeffff97a1;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffdf5b000041b0;
  *((unsigned long *)&__m256i_op1[1]) = 0xfffffffeffff97a1;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffdf5b000041b0;
  *((unsigned long *)&__m256i_result[3]) = 0x000000010000685e;
  *((unsigned long *)&__m256i_result[2]) = 0x000020a4ffffbe4f;
  *((unsigned long *)&__m256i_result[1]) = 0x000000010000685e;
  *((unsigned long *)&__m256i_result[0]) = 0x000020a4ffffbe4f;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000070002000a;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000070002000a;
  *((unsigned long *)&__m256i_op1[3]) = 0x0040000000000003;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_op1[1]) = 0x0040000000000003;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[3]) = 0xffbffffffffffffe;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffffa;
  *((unsigned long *)&__m256i_result[1]) = 0xffbffffffffffffe;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffffa;
  __m256i_out = __lasx_xvorn_v (__m256i_op0, __m256i_op1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

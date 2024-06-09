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

  *((unsigned long *)&__m256i_op0[3]) = 0x8000800000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000800080000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xc9d8080067f50020;
  *((unsigned long *)&__m256i_op0[0]) = 0xc70000020000c000;
  *((unsigned long *)&__m256i_result[3]) = 0xf000f00000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000f000f0000000;
  *((unsigned long *)&__m256i_result[1]) = 0xf0f008000ff5000f;
  *((unsigned long *)&__m256i_result[0]) = 0xf00000020000f000;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000ff80;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000fff8;
  *((unsigned long *)&__m256i_result[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000ffff;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x000e000e000e000e;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000e000e000e000e;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000e000e000e000e;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000e000e000e000e;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00003fea00013fec;
  *((unsigned long *)&__m256i_op0[2]) = 0x00003fe50001c013;
  *((unsigned long *)&__m256i_op0[1]) = 0x00003fea00013fec;
  *((unsigned long *)&__m256i_op0[0]) = 0x00003fe50001c013;
  *((unsigned long *)&__m256i_result[3]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[2]) = 0x000000ff0000ff00;
  *((unsigned long *)&__m256i_result[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_result[0]) = 0x000000ff0000ff00;
  __m256i_out = __lasx_xvsat_b (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000399400003994;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000399400003994;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000399400003994;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000399400003994;
  *((unsigned long *)&__m256i_result[3]) = 0x00000fff00000fff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000fff00000fff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000fff00000fff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000fff00000fff;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xc);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfebdff3eff3dff52;
  *((unsigned long *)&__m256i_op0[2]) = 0xfebdff3eff3dff52;
  *((unsigned long *)&__m256i_op0[1]) = 0xfebdff3eff3dff52;
  *((unsigned long *)&__m256i_op0[0]) = 0xfebdff3eff3dff52;
  *((unsigned long *)&__m256i_result[3]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_result[2]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_result[1]) = 0xffc0ffc0ffc0ffc0;
  *((unsigned long *)&__m256i_result[0]) = 0xffc0ffc0ffc0ffc0;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x1);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00002df900001700;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffe05ffffe911;
  *((unsigned long *)&__m256i_op0[1]) = 0x00002df900001700;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffe05ffffe911;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000300000003;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffcfffffffc;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000300000003;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffcfffffffc;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffffdd97dc4;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffffffffdd97dc4;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffdd97dc4;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffdd97dc4;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000014402080144;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000014402080144;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000007f007f007f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000007f007f007f;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000ffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000ffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000003fffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000003fffff;
  __m256i_out = __lasx_xvsat_h (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0002000200000022;
  *((unsigned long *)&__m256i_op0[0]) = 0x0049004200000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000007f00000022;
  *((unsigned long *)&__m256i_result[0]) = 0x0000007f00000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffffffefffffefd;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffefffffefd;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x9);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00c200c200c200c2;
  *((unsigned long *)&__m256i_op0[2]) = 0x00c200c200c200bb;
  *((unsigned long *)&__m256i_op0[1]) = 0x00c200c200c200c2;
  *((unsigned long *)&__m256i_op0[0]) = 0x00c200c200c200bb;
  *((unsigned long *)&__m256i_result[3]) = 0x007fffff007fffff;
  *((unsigned long *)&__m256i_result[2]) = 0x007fffff007fffff;
  *((unsigned long *)&__m256i_result[1]) = 0x007fffff007fffff;
  *((unsigned long *)&__m256i_result[0]) = 0x007fffff007fffff;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x17);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0020002000200020;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0001ffff0001ffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0001ffff0001ffff;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000080000001000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000080000000800;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000080000001000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000080000000800;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000f0000000f;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x4);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op0[2]) = 0x000b8f81b8c840e4;
  *((unsigned long *)&__m256i_op0[1]) = 0x000050504c4c2362;
  *((unsigned long *)&__m256i_op0[0]) = 0x000b8f81b8c840e4;
  *((unsigned long *)&__m256i_result[3]) = 0x000007ff000007ff;
  *((unsigned long *)&__m256i_result[2]) = 0x000007fffffff800;
  *((unsigned long *)&__m256i_result[1]) = 0x000007ff000007ff;
  *((unsigned long *)&__m256i_result[0]) = 0x000007fffffff800;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x11);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_w (__m256i_op0, 0xf);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x97541c5897541c58;
  *((unsigned long *)&__m256i_op0[2]) = 0x97541c5897541c58;
  *((unsigned long *)&__m256i_op0[1]) = 0x97541c5897541c58;
  *((unsigned long *)&__m256i_op0[0]) = 0x97541c5897541c58;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffc00000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffc00000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffc00000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffc00000000;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x22);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffff5f5c;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffff605a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffff5f5c;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffff605a;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffff5f5c;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffff605a;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffff5f5c;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffff605a;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x2d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x001175f10e4330e8;
  *((unsigned long *)&__m256i_op0[2]) = 0xff8f0842ff29211e;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffff8d9ffa7103d;
  *((unsigned long *)&__m256i_result[3]) = 0x001175f10e4330e8;
  *((unsigned long *)&__m256i_result[2]) = 0xff8f0842ff29211e;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffff8d9ffa7103d;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x39);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000001;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x21);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_op0[2]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_op0[1]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_op0[0]) = 0xc2c2c2c2c2c2c2c2;
  *((unsigned long *)&__m256i_result[3]) = 0xfffe000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xfffe000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0xfffe000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffe000000000000;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x31);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[3]) = 0x00000001ffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x00000001ffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x00000001ffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x00000001ffffffff;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x21);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x3d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x498000804843ffe0;
  *((unsigned long *)&__m256i_op0[2]) = 0x4980008068400000;
  *((unsigned long *)&__m256i_op0[1]) = 0x498000804843ffe0;
  *((unsigned long *)&__m256i_op0[0]) = 0x4980008068400000;
  *((unsigned long *)&__m256i_result[3]) = 0x0fffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0x0fffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0fffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0x0fffffffffffffff;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x3c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffeb6839ffffd80;
  *((unsigned long *)&__m256i_op0[2]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffeb6839ffffd80;
  *((unsigned long *)&__m256i_op0[0]) = 0xfffe97c020010001;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256i_result[1]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff8;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000002c21ffeff;
  *((unsigned long *)&__m256i_op0[2]) = 0xc0000000c0000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000002c21ffeff;
  *((unsigned long *)&__m256i_op0[0]) = 0xc0000000c0000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[2]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffffffffff8;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x3);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvsat_d (__m256i_op0, 0x32);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

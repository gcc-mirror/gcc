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
  *((unsigned long *)&__m256i_op0[2]) = 0x0003ff540000081c;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0003ffd00003fd38;
  *((unsigned long *)&__m256i_op1[3]) = 0x0001ffaa0000040e;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000716800007bb6;
  *((unsigned long *)&__m256i_op1[1]) = 0x0001ffe80001fe9c;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000228200001680;
  *((unsigned long *)&__m256i_op2[3]) = 0x372e9d75e8aab100;
  *((unsigned long *)&__m256i_op2[2]) = 0xc5c085372cfabfba;
  *((unsigned long *)&__m256i_op2[1]) = 0x31730b5beb7c99f5;
  *((unsigned long *)&__m256i_op2[0]) = 0x0658f2dc0eb21e3c;
  *((unsigned long *)&__m256i_result[3]) = 0x002e4db200000000;
  *((unsigned long *)&__m256i_result[2]) = 0x000315ac0000d658;
  *((unsigned long *)&__m256i_result[1]) = 0x00735278007cf94c;
  *((unsigned long *)&__m256i_result[0]) = 0x0003ed8800031b38;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[2]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[1]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[0]) = 0xff01ff01ff01ff01;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffff0001ff04;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffff02a0fefc;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000000cfefd;
  *((unsigned long *)&__m256i_op1[3]) = 0x6100000800060005;
  *((unsigned long *)&__m256i_op1[2]) = 0x5ee1c073b800c916;
  *((unsigned long *)&__m256i_op1[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x5ff00007fff9fff3;
  *((unsigned long *)&__m256i_op2[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op2[2]) = 0xfffffffefffffefc;
  *((unsigned long *)&__m256i_op2[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op2[0]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_result[3]) = 0xfffffff7fffbfefa;
  *((unsigned long *)&__m256i_result[2]) = 0xff1eff1902a0fea4;
  *((unsigned long *)&__m256i_result[1]) = 0xff10000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xff10fff9ff13fd17;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfafafafafafafafa;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xfefefefefefefefe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000fefefe;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xf9fbf9fbf9fbf9fb;
  *((unsigned long *)&__m256i_result[2]) = 0xff01ff01ff01ff01;
  *((unsigned long *)&__m256i_result[1]) = 0xfdfffdfffdfffdff;
  *((unsigned long *)&__m256i_result[0]) = 0xff01ff01fffffdff;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000003fff3fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000003fff3fff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000627;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000627;
  *((unsigned long *)&__m256i_op2[3]) = 0x7fff7fff05407fff;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x7fff7fff05407fff;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[2]) = 0x000000003fff3fff;
  *((unsigned long *)&__m256i_result[1]) = 0x3fff3fff3fff3fff;
  *((unsigned long *)&__m256i_result[0]) = 0x000000003fff3fff;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000400;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000400;
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
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x003f003f003f003f;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x003f003f003f003f;
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
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffee;
  *((unsigned long *)&__m256i_result[3]) = 0xff00ff00ff00ef32;
  *((unsigned long *)&__m256i_result[2]) = 0xff00ff00ff00ef32;
  *((unsigned long *)&__m256i_result[1]) = 0xff00ff00ff00ef32;
  *((unsigned long *)&__m256i_result[0]) = 0xff00ff00ff00ef32;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x1010101010101010;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op2[3]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_op2[2]) = 0xfffffffffdd97dc4;
  *((unsigned long *)&__m256i_op2[1]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_op2[0]) = 0xfffffffffdd97dc4;
  *((unsigned long *)&__m256i_result[3]) = 0x0000ffff00000001;
  *((unsigned long *)&__m256i_result[2]) = 0x1010100f10100fd4;
  *((unsigned long *)&__m256i_result[1]) = 0x0000ffff00000001;
  *((unsigned long *)&__m256i_result[0]) = 0x1010100f10100fd4;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_op0[2]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_op0[1]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_op0[0]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[2]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[1]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[0]) = 0xebebebebebebebeb;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x001f001fffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffe0ffe000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x001f001fffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffe0ffe000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffff0000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_h_bu_b (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x34ec5670cd4b5ec0;
  *((unsigned long *)&__m256i_op0[2]) = 0x4f111e4b8e0d7291;
  *((unsigned long *)&__m256i_op0[1]) = 0xeaa81f47dc3bdd09;
  *((unsigned long *)&__m256i_op0[0]) = 0x0e0d5fde5df99830;
  *((unsigned long *)&__m256i_op1[3]) = 0x67390c19e4b17547;
  *((unsigned long *)&__m256i_op1[2]) = 0xbacda0f96d2cec01;
  *((unsigned long *)&__m256i_op1[1]) = 0xee20ad1adae2cc16;
  *((unsigned long *)&__m256i_op1[0]) = 0x5a2003c6a406fe53;
  *((unsigned long *)&__m256i_op2[3]) = 0x80c72fcd40fb3bc0;
  *((unsigned long *)&__m256i_op2[2]) = 0x84bd087966d4ace0;
  *((unsigned long *)&__m256i_op2[1]) = 0x26aa68b274dc1322;
  *((unsigned long *)&__m256i_op2[0]) = 0xe072db2bb9d4cd40;
  *((unsigned long *)&__m256i_result[3]) = 0x372e9d75e8aab100;
  *((unsigned long *)&__m256i_result[2]) = 0x5464fbfc416b9f71;
  *((unsigned long *)&__m256i_result[1]) = 0x31730b5beb7c99f5;
  *((unsigned long *)&__m256i_result[0]) = 0x0d8264202b8ea3f0;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff0000ffff00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0xff000000ffffff00;
  *((unsigned long *)&__m256i_op0[1]) = 0x000000ff000000ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffffffff00ff;
  *((unsigned long *)&__m256i_op1[3]) = 0x01fa022a01a401e5;
  *((unsigned long *)&__m256i_op1[2]) = 0x030d03aa0079029b;
  *((unsigned long *)&__m256i_op1[1]) = 0x024c01f901950261;
  *((unsigned long *)&__m256i_op1[0]) = 0x008102c2008a029f;
  *((unsigned long *)&__m256i_op2[3]) = 0x002e4db200000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x000315ac0000d658;
  *((unsigned long *)&__m256i_op2[1]) = 0x00735278007cf94c;
  *((unsigned long *)&__m256i_op2[0]) = 0x0003ed8800031b38;
  *((unsigned long *)&__m256i_result[3]) = 0x01a72334ffff00ff;
  *((unsigned long *)&__m256i_result[2]) = 0xff4f6838ff937648;
  *((unsigned long *)&__m256i_result[1]) = 0x00a2afb7fff00ecb;
  *((unsigned long *)&__m256i_result[0]) = 0xffce110f004658c7;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000003a099512;
  *((unsigned long *)&__m256i_op0[1]) = 0x280ac9da313763f5;
  *((unsigned long *)&__m256i_op0[0]) = 0xe032c738adcc6bbf;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0xfffe000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000ffff00010000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0001000100020001;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000fffffffffffe;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffffffff0001;
  *((unsigned long *)&__m256i_result[2]) = 0x000000003a099512;
  *((unsigned long *)&__m256i_result[1]) = 0x280ac9da313763f5;
  *((unsigned long *)&__m256i_result[0]) = 0xe032c738adcc6bbf;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7f00000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x7f00000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x7fff000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x7fff000000000000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_op0[2]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_op0[1]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_op0[0]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_op1[3]) = 0x000000000045f3fb;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x000000000045f3fb;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[2]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[1]) = 0xf7f7f7f7f7f7f7f7;
  *((unsigned long *)&__m256i_result[0]) = 0xf7f7f7f7f7f7f7f7;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff00010003;
  *((unsigned long *)&__m256i_op0[1]) = 0x0080000200000002;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff00010003;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000200000002;
  *((unsigned long *)&__m256i_result[2]) = 0x0000ffff00010003;
  *((unsigned long *)&__m256i_result[1]) = 0x0080000200000002;
  *((unsigned long *)&__m256i_result[0]) = 0x0000ffff00010003;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x000000007fffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000001a00;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000001a00;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000ffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000001f0000001f;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000001f0000ffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000060008;
  *((unsigned long *)&__m256i_op2[2]) = 0x00000000000c005b;
  *((unsigned long *)&__m256i_op2[1]) = 0xfffffffffffe0000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000040053;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffff0007fff7;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffff005affa4;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffe100000000;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000053ffac;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op2[2]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op2[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op2[0]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000100000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000100000000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x5fff5fff607f0000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x5fff5fff607f0000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x5fff5fff607f0000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x5fff5fff607f0000;
  *((unsigned long *)&__m256i_op2[3]) = 0x1000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x1000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_result[2]) = 0x5fff5fff607f0000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000420080000000;
  *((unsigned long *)&__m256i_result[0]) = 0x5fff5fff607f0000;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
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
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op0[2]) = 0x0100004300000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op0[0]) = 0x0100004300000000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op2[2]) = 0xff00010001000100;
  *((unsigned long *)&__m256i_op2[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_op2[0]) = 0xff00010001000100;
  *((unsigned long *)&__m256i_result[3]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[2]) = 0x01ffff4300ffff00;
  *((unsigned long *)&__m256i_result[1]) = 0x0100010001000100;
  *((unsigned long *)&__m256i_result[0]) = 0x01ffff4300ffff00;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000001000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000001000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_op1[3]) = 0xffff0607ffff0607;
  *((unsigned long *)&__m256i_op1[2]) = 0xffff0607ffff0607;
  *((unsigned long *)&__m256i_op1[1]) = 0xffff0607ffff0607;
  *((unsigned long *)&__m256i_op1[0]) = 0xffff0607ffff0607;
  *((unsigned long *)&__m256i_op2[3]) = 0x00000000f9f9f9f9;
  *((unsigned long *)&__m256i_op2[2]) = 0x00000000faf3f3f2;
  *((unsigned long *)&__m256i_op2[1]) = 0x00000000f9f9f9f9;
  *((unsigned long *)&__m256i_op2[0]) = 0x00000000faf3f3f2;
  *((unsigned long *)&__m256i_result[3]) = 0x00000000ffdbbbcf;
  *((unsigned long *)&__m256i_result[2]) = 0x00000000ffb8579f;
  *((unsigned long *)&__m256i_result[1]) = 0x00000000ffdbbbcf;
  *((unsigned long *)&__m256i_result[0]) = 0x00000000ffb8579f;
  __m256i_out
      = __lasx_xvmaddwev_w_hu_h (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op2[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op2[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[0]) = 0xffffffffffffffff;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x009200f200840080;
  *((unsigned long *)&__m256i_op0[2]) = 0x009200f200840080;
  *((unsigned long *)&__m256i_op0[1]) = 0x00b200b300800080;
  *((unsigned long *)&__m256i_op0[0]) = 0x00b200b300800080;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000004000000040;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x009200f200840080;
  *((unsigned long *)&__m256i_result[2]) = 0x009200f200840080;
  *((unsigned long *)&__m256i_result[1]) = 0x00b200b300800080;
  *((unsigned long *)&__m256i_result[0]) = 0x00b200b300800080;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000202020;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000404040;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000202020;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000404040;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000202020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000404040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000202020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000404040;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256i_op1[3]) = 0x0000000000800080;
  *((unsigned long *)&__m256i_op1[2]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_op1[1]) = 0x0000000000800080;
  *((unsigned long *)&__m256i_op1[0]) = 0x0000000000000202;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x1fa0000000080000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x1fa0000000080000;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_op1[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op1[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op2[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op2[0]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[2]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[1]) = 0xffffffff00000000;
  *((unsigned long *)&__m256i_result[0]) = 0xfffffffe00000001;
  __m256i_out
      = __lasx_xvmaddwev_d_wu_w (__m256i_op0, __m256i_op1, __m256i_op2);
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

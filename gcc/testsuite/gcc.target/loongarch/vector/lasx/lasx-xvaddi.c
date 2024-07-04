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
  *((unsigned long *)&__m256i_op0[1]) = 0x44bb2cd3a35c2fd0;
  *((unsigned long *)&__m256i_op0[0]) = 0xca355ba46a95e31c;
  *((unsigned long *)&__m256i_result[3]) = 0x1d1d1d1d1d1d1d1d;
  *((unsigned long *)&__m256i_result[2]) = 0x1d1d1d1d1d1d1d1d;
  *((unsigned long *)&__m256i_result[1]) = 0x61d849f0c0794ced;
  *((unsigned long *)&__m256i_result[0]) = 0xe75278c187b20039;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffbf7f7fff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffe651bfff;
  *((unsigned long *)&__m256i_result[3]) = 0x1d1d1d1d1d1d1d1d;
  *((unsigned long *)&__m256i_result[2]) = 0x1d1d1d1ddd9d9d1d;
  *((unsigned long *)&__m256i_result[1]) = 0x1d1d1d1d1d1d1d1d;
  *((unsigned long *)&__m256i_result[0]) = 0x1d1d1d1d046fdd1d;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1515151515151515;
  *((unsigned long *)&__m256i_result[2]) = 0x1515151515151515;
  *((unsigned long *)&__m256i_result[1]) = 0x1515151515151515;
  *((unsigned long *)&__m256i_result[0]) = 0x1515151515151515;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1818181818181818;
  *((unsigned long *)&__m256i_result[2]) = 0x1818181818181818;
  *((unsigned long *)&__m256i_result[1]) = 0x1818181818181818;
  *((unsigned long *)&__m256i_result[0]) = 0x1818181818181818;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x18);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00007fff00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00007fff00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[2]) = 0x0202810102020202;
  *((unsigned long *)&__m256i_result[1]) = 0x0202020202020202;
  *((unsigned long *)&__m256i_result[0]) = 0x0202810102020202;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[2]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[1]) = 0x0909090909090909;
  *((unsigned long *)&__m256i_result[0]) = 0x0909090909090909;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0xa);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[2]) = 0x00ff00ff00ffce20;
  *((unsigned long *)&__m256i_op0[1]) = 0x00ff00ff00ff00ff;
  *((unsigned long *)&__m256i_op0[0]) = 0x00ff00ff00ffce20;
  *((unsigned long *)&__m256i_result[3]) = 0x1514151415141514;
  *((unsigned long *)&__m256i_result[2]) = 0x151415141514e335;
  *((unsigned long *)&__m256i_result[1]) = 0x1514151415141514;
  *((unsigned long *)&__m256i_result[0]) = 0x151415141514e335;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x15);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0606060606060606;
  *((unsigned long *)&__m256i_result[2]) = 0x0606060606060606;
  *((unsigned long *)&__m256i_result[1]) = 0x0606060606060606;
  *((unsigned long *)&__m256i_result[0]) = 0x0606060606060606;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x1212121212121212;
  *((unsigned long *)&__m256i_result[2]) = 0x1212121212121212;
  *((unsigned long *)&__m256i_result[1]) = 0x1212121212121212;
  *((unsigned long *)&__m256i_result[0]) = 0x1212121212121212;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x12);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000001200000012;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000001200000012;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000001200000012;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000001200000012;
  *((unsigned long *)&__m256i_result[3]) = 0x1a1a1a2c1a1a1a2c;
  *((unsigned long *)&__m256i_result[2]) = 0x1a1a1a2c1a1a1a2c;
  *((unsigned long *)&__m256i_result[1]) = 0x1a1a1a2c1a1a1a2c;
  *((unsigned long *)&__m256i_result[0]) = 0x1a1a1a2c1a1a1a2c;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000100000001;
  *((unsigned long *)&__m256i_result[3]) = 0x1d1d1d1e1d1d1d1e;
  *((unsigned long *)&__m256i_result[2]) = 0x1d1d1d1e1d1d1d1e;
  *((unsigned long *)&__m256i_result[1]) = 0x1d1d1d1e1d1d1d1e;
  *((unsigned long *)&__m256i_result[0]) = 0x1d1d1d1e1d1d1d1e;
  __m256i_out = __lasx_xvaddi_bu (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x5980000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x5980000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[2]) = 0x5982000200020002;
  *((unsigned long *)&__m256i_result[1]) = 0x0002000200020002;
  *((unsigned long *)&__m256i_result[0]) = 0x5982000200020002;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x2);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000002a54290;
  *((unsigned long *)&__m256i_result[3]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_result[2]) = 0x001f001f02c442af;
  *((unsigned long *)&__m256i_result[1]) = 0x001f001f001f001f;
  *((unsigned long *)&__m256i_result[0]) = 0x001f001f02c442af;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x1f);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100010;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x807e80fd80fe80fd;
  *((unsigned long *)&__m256i_op0[2]) = 0x80938013800d8002;
  *((unsigned long *)&__m256i_op0[1]) = 0x807e80fd80fe0000;
  *((unsigned long *)&__m256i_op0[0]) = 0x80938013800d0005;
  *((unsigned long *)&__m256i_result[3]) = 0x8091811081118110;
  *((unsigned long *)&__m256i_result[2]) = 0x80a6802680208015;
  *((unsigned long *)&__m256i_result[1]) = 0x8091811081110013;
  *((unsigned long *)&__m256i_result[0]) = 0x80a6802680200018;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000003f00390035;
  *((unsigned long *)&__m256i_op0[2]) = 0x8015003f0006001f;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000003f00390035;
  *((unsigned long *)&__m256i_op0[0]) = 0x8015003f0006001f;
  *((unsigned long *)&__m256i_result[3]) = 0x000b004a00440040;
  *((unsigned long *)&__m256i_result[2]) = 0x8020004a0011002a;
  *((unsigned long *)&__m256i_result[1]) = 0x000b004a00440040;
  *((unsigned long *)&__m256i_result[0]) = 0x8020004a0011002a;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0xb);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0016001600160016;
  *((unsigned long *)&__m256i_result[2]) = 0x0016001600160016;
  *((unsigned long *)&__m256i_result[1]) = 0x0016001600160016;
  *((unsigned long *)&__m256i_result[0]) = 0x0016001600160016;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x16);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xa1a1a1a1a1a1a1a1;
  *((unsigned long *)&__m256i_op0[2]) = 0xa1a1a1a15e5e5e5e;
  *((unsigned long *)&__m256i_op0[1]) = 0xa1a1a1a1a1a1a1a1;
  *((unsigned long *)&__m256i_op0[0]) = 0xa1a1a1a15e5e5e5e;
  *((unsigned long *)&__m256i_result[3]) = 0xa1bfa1bfa1bfa1bf;
  *((unsigned long *)&__m256i_result[2]) = 0xa1bfa1bf5e7c5e7c;
  *((unsigned long *)&__m256i_result[1]) = 0xa1bfa1bfa1bfa1bf;
  *((unsigned long *)&__m256i_result[0]) = 0xa1bfa1bf5e7c5e7c;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000100080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000080;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000100080;
  *((unsigned long *)&__m256i_result[3]) = 0x001a001a001a009a;
  *((unsigned long *)&__m256i_result[2]) = 0x001a001a002a009a;
  *((unsigned long *)&__m256i_result[1]) = 0x001a001a001a009a;
  *((unsigned long *)&__m256i_result[0]) = 0x001a001a002a009a;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000010001;
  *((unsigned long *)&__m256i_result[3]) = 0x001c001c001c001c;
  *((unsigned long *)&__m256i_result[2]) = 0x001c001c001c001c;
  *((unsigned long *)&__m256i_result[1]) = 0x001c001c001c001c;
  *((unsigned long *)&__m256i_result[0]) = 0x001c001c001d001d;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x1c);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x7200000072000000;
  *((unsigned long *)&__m256i_result[3]) = 0x721e001e721e001e;
  *((unsigned long *)&__m256i_result[2]) = 0x721e001e721e001e;
  *((unsigned long *)&__m256i_result[1]) = 0x721e001e721e001e;
  *((unsigned long *)&__m256i_result[0]) = 0x721e001e721e001e;
  __m256i_out = __lasx_xvaddi_hu (__m256i_op0, 0x1e);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001900000019;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000600000006;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001a0000001a;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001a0000001a;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x1a);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001900000019;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001900000019;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x19);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001d0000001d;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001d0000001d;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001d0000001d;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001d0000001d;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x1d);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000600000006;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x6);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000001fffd;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000001fffd;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000700020004;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000600000006;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000700020004;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000800000008;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000800000008;
  __m256i_out = __lasx_xvaddi_wu (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x000019410000e69a;
  *((unsigned long *)&__m256i_op0[2]) = 0xf259905a0c126604;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000883a00000f20;
  *((unsigned long *)&__m256i_op0[0]) = 0x6d3c2d3aa1c82947;
  *((unsigned long *)&__m256i_result[3]) = 0x000019410000e6aa;
  *((unsigned long *)&__m256i_result[2]) = 0xf259905a0c126614;
  *((unsigned long *)&__m256i_result[1]) = 0x0000883a00000f30;
  *((unsigned long *)&__m256i_result[0]) = 0x6d3c2d3aa1c82957;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0x10);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000000d;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000000d;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000000d;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000000d;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0xd);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfff0fff0ff01ff01;
  *((unsigned long *)&__m256i_op0[2]) = 0xfff0fff0fff0fff0;
  *((unsigned long *)&__m256i_op0[1]) = 0xfff0fff0ff01ff01;
  *((unsigned long *)&__m256i_op0[0]) = 0xfff0fff0fff0fff0;
  *((unsigned long *)&__m256i_result[3]) = 0xfff0fff0ff01ff14;
  *((unsigned long *)&__m256i_result[2]) = 0xfff0fff0fff10003;
  *((unsigned long *)&__m256i_result[1]) = 0xfff0fff0ff01ff14;
  *((unsigned long *)&__m256i_result[0]) = 0xfff0fff0fff10003;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0x13);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000504fffff3271;
  *((unsigned long *)&__m256i_op0[2]) = 0xffff47b4ffff5879;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000504fffff3271;
  *((unsigned long *)&__m256i_op0[0]) = 0xffff47b4ffff5879;
  *((unsigned long *)&__m256i_result[3]) = 0x0000504fffff3271;
  *((unsigned long *)&__m256i_result[2]) = 0xffff47b4ffff5879;
  *((unsigned long *)&__m256i_result[1]) = 0x0000504fffff3271;
  *((unsigned long *)&__m256i_result[0]) = 0xffff47b4ffff5879;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000008;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000008;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0x8);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0fffffff0fffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0fffffff10000006;
  *((unsigned long *)&__m256i_result[2]) = 0x0fffffff10000006;
  *((unsigned long *)&__m256i_result[1]) = 0x0fffffff10000006;
  *((unsigned long *)&__m256i_result[0]) = 0x0fffffff10000006;
  __m256i_out = __lasx_xvaddi_du (__m256i_op0, 0x7);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

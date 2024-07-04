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

  *((unsigned long *)&__m256i_op0[3]) = 0x04481940fbb7e6bf;
  *((unsigned long *)&__m256i_op0[2]) = 0xf2781966e6991966;
  *((unsigned long *)&__m256i_op0[1]) = 0x51258839aeda77c6;
  *((unsigned long *)&__m256i_op0[0]) = 0xcf25f0e00f1ff0e0;
  *((unsigned long *)&__m256i_result[3]) = 0x0501030100000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0001030100000301;
  *((unsigned long *)&__m256i_result[1]) = 0x0102000200000100;
  *((unsigned long *)&__m256i_result[0]) = 0x0002000004030000;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0001000100010001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000f0000000f;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000f0000000f;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000003868686a20;
  *((unsigned long *)&__m256i_op0[2]) = 0x0045b8ae81bce1d8;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000003868686a20;
  *((unsigned long *)&__m256i_op0[0]) = 0x0045b8ae81bce1d8;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001a00000001;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000900000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001a00000001;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000900000000;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100010;
  __m256i_out = __lasx_xvclz_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_op0[1]) = 0xfffffffffffffffe;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000001;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080807;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080807;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000007fff;
  *((unsigned long *)&__m256i_op0[1]) = 0x7fffffff7fffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000007fff;
  *((unsigned long *)&__m256i_result[3]) = 0x0001000000010000;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100001;
  *((unsigned long *)&__m256i_result[1]) = 0x0001000000010000;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100001;
  __m256i_out = __lasx_xvclz_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xff00ffffff00ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xff00ffffff00ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0008000000080000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0008000000080000;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x0010001000100010;
  __m256i_out = __lasx_xvclz_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x00000000000000fe;
  *((unsigned long *)&__m256i_op0[2]) = 0x000000000000007f;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000002;
  *((unsigned long *)&__m256i_op0[0]) = 0x000000000000007f;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000018;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000019;
  *((unsigned long *)&__m256i_result[1]) = 0x000000200000001e;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000019;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0b085bfc00000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0b004bc000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0b085bfc00000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0b004bc000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0404010008080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0408010008080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0404010008080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0408010008080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffc040ffffc09d;
  *((unsigned long *)&__m256i_op0[2]) = 0x00003fc00000428a;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffc040ffffc09d;
  *((unsigned long *)&__m256i_op0[0]) = 0x00003fc00000428a;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000012;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000012;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0404010008080808;
  *((unsigned long *)&__m256i_op0[2]) = 0x0408010008080808;
  *((unsigned long *)&__m256i_op0[1]) = 0x0404010008080808;
  *((unsigned long *)&__m256i_op0[0]) = 0x0408010008080808;
  *((unsigned long *)&__m256i_result[3]) = 0x0505070804040404;
  *((unsigned long *)&__m256i_result[2]) = 0x0504070804040404;
  *((unsigned long *)&__m256i_result[1]) = 0x0505070804040404;
  *((unsigned long *)&__m256i_result[0]) = 0x0504070804040404;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000ffff0000ffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[2]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[1]) = 0x0000001000000010;
  *((unsigned long *)&__m256i_result[0]) = 0x0000001000000010;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0006ffff0004ffff;
  *((unsigned long *)&__m256i_op0[2]) = 0x0002ffff0000ff00;
  *((unsigned long *)&__m256i_op0[1]) = 0x0006ffff0004ffff;
  *((unsigned long *)&__m256i_op0[0]) = 0x0002ffff0000ff00;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000000d;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000000e;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000000d;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000000e;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000032;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x000003c000000032;
  *((unsigned long *)&__m256i_result[3]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[2]) = 0x001000100010000a;
  *((unsigned long *)&__m256i_result[1]) = 0x0010001000100010;
  *((unsigned long *)&__m256i_result[0]) = 0x001000060010000a;
  __m256i_out = __lasx_xvclz_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0003800400038004;
  *((unsigned long *)&__m256i_op0[2]) = 0x000a800b000a800b;
  *((unsigned long *)&__m256i_op0[1]) = 0x0003800400038004;
  *((unsigned long *)&__m256i_op0[0]) = 0x000a800b000a800b;
  *((unsigned long *)&__m256i_result[3]) = 0x000000000000000e;
  *((unsigned long *)&__m256i_result[2]) = 0x000000000000000c;
  *((unsigned long *)&__m256i_result[1]) = 0x000000000000000e;
  *((unsigned long *)&__m256i_result[0]) = 0x000000000000000c;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffff00000080;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffff00000080;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000008080800;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000008080800;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0a0a000000000a0a;
  *((unsigned long *)&__m256i_op0[2]) = 0x0a0a0a0a00000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0a0a000000000a0a;
  *((unsigned long *)&__m256i_op0[0]) = 0x0a0a0a0a00000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0004001000100004;
  *((unsigned long *)&__m256i_result[2]) = 0x0004000400100010;
  *((unsigned long *)&__m256i_result[1]) = 0x0004001000100004;
  *((unsigned long *)&__m256i_result[0]) = 0x0004000400100010;
  __m256i_out = __lasx_xvclz_h (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000000;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000ff88ff88;
  *((unsigned long *)&__m256i_op0[1]) = 0x8000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000ff88ff88;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000020;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x00000000007f8000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x00000000007f8000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000029;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000029;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[2]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[1]) = 0x0000002000000020;
  *((unsigned long *)&__m256i_result[0]) = 0x0000002000000020;
  __m256i_out = __lasx_xvclz_w (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0101010101010101;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000007;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000007;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000001010000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000001010000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000027;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000027;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[2]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[1]) = 0x0000000000000040;
  *((unsigned long *)&__m256i_result[0]) = 0x0000000000000040;
  __m256i_out = __lasx_xvclz_d (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256i_result[3]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[2]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[1]) = 0x0808080808080808;
  *((unsigned long *)&__m256i_result[0]) = 0x0808080808080808;
  __m256i_out = __lasx_xvclz_b (__m256i_op0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

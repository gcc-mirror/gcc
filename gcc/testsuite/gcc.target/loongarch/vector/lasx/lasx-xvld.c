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

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_result[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_result[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_result[0]) = 0x0ad152a5ad72feeb;
  __m256i_out = __lasx_xvld ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_result[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_result[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_result[0]) = 0x0ad152a5ad72feeb;
  __m256i_out = __lasx_xvldx ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[2]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[1]) = 0xebebebebebebebeb;
  *((unsigned long *)&__m256i_result[0]) = 0xebebebebebebebeb;
  __m256i_out = __lasx_xvldrepl_b ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0xfeebfeebfeebfeeb;
  *((unsigned long *)&__m256i_result[2]) = 0xfeebfeebfeebfeeb;
  *((unsigned long *)&__m256i_result[1]) = 0xfeebfeebfeebfeeb;
  *((unsigned long *)&__m256i_result[0]) = 0xfeebfeebfeebfeeb;
  __m256i_out = __lasx_xvldrepl_h ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0xad72feebad72feeb;
  *((unsigned long *)&__m256i_result[2]) = 0xad72feebad72feeb;
  *((unsigned long *)&__m256i_result[1]) = 0xad72feebad72feeb;
  *((unsigned long *)&__m256i_result[0]) = 0xad72feebad72feeb;
  __m256i_out = __lasx_xvldrepl_w ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  *((unsigned long *)&__m256i_op0[3]) = 0x042f0500cfea969a;
  *((unsigned long *)&__m256i_op0[2]) = 0x58569d7be9179100;
  *((unsigned long *)&__m256i_op0[1]) = 0xa98d4f7a77c308ee;
  *((unsigned long *)&__m256i_op0[0]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[3]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[2]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[1]) = 0x0ad152a5ad72feeb;
  *((unsigned long *)&__m256i_result[0]) = 0x0ad152a5ad72feeb;
  __m256i_out = __lasx_xvldrepl_d ((unsigned long *)&__m256i_op0, 0x0);
  ASSERTEQ_64 (__LINE__, __m256i_result, __m256i_out);

  return 0;
}

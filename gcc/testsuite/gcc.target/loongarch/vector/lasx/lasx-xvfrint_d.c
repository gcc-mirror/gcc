/* { dg-options "-mlasx -w -fno-strict-aliasing" } */
/* { dg-timeout 500 } */
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

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x0);
  *((unsigned long *)&__m256d_op0[3]) = 0xfffefffe00000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xfffefffefffefffd;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0xfffefffe00000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xfffefffefffefffd;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000008050501;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000008050501;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000008;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000008;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000008;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000008;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_op0[2]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_op0[1]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_op0[0]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_result[3]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_result[2]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_result[1]) = 0xfffffffffffffff8;
  *((unsigned long *)&__m256d_result[0]) = 0xfffffffffffffff8;
  __m256d_out = __lasx_xvfrint_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000080008001;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000080008001;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x7c00000880008000;
  *((unsigned long *)&__m256d_op0[1]) = 0x00000000ffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x7c00000880008000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7c00000880008000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7c00000880008000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x6018000000000cd1;
  *((unsigned long *)&__m256d_op0[2]) = 0x6040190d00000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x6018000000000cd1;
  *((unsigned long *)&__m256d_op0[0]) = 0x6040190d00000000;
  *((unsigned long *)&__m256d_result[3]) = 0x6018000000000cd1;
  *((unsigned long *)&__m256d_result[2]) = 0x6040190d00000000;
  *((unsigned long *)&__m256d_result[1]) = 0x6018000000000cd1;
  *((unsigned long *)&__m256d_result[0]) = 0x6040190d00000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x3eab77367fff4848;
  *((unsigned long *)&__m256d_op0[2]) = 0x408480007fff0000;
  *((unsigned long *)&__m256d_op0[1]) = 0x3eab77367fff4848;
  *((unsigned long *)&__m256d_op0[0]) = 0x408480007fff0000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x4084800000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x4084800000000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_op0[2]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_op0[1]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_op0[0]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_result[3]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_result[2]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_result[1]) = 0xffff0001ffff0001;
  *((unsigned long *)&__m256d_result[0]) = 0xffff0001ffff0001;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x3fffbfff80000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x00004000007f8000;
  *((unsigned long *)&__m256d_op0[1]) = 0x3fffbfff80000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x00004000007f8000;
  *((unsigned long *)&__m256d_result[3]) = 0x4000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x4000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrne_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000ffff00010000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000ffff00010000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x3ff0000000000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xfffffefefffffefe;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xfffffefe00000000;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xfffffefefffffefe;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xfffffefe00000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x000100da000100fd;
  *((unsigned long *)&__m256d_op0[2]) = 0x0001ffe20001fefd;
  *((unsigned long *)&__m256d_op0[1]) = 0x0001009a000100fd;
  *((unsigned long *)&__m256d_op0[0]) = 0x0001ff640001fefd;
  *((unsigned long *)&__m256d_result[3]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x3ff0000000000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m256d_op0[2]) = 0x01fc03fc01fc03fc;
  *((unsigned long *)&__m256d_op0[1]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m256d_op0[0]) = 0x01fc03fc01fc03fc;
  *((unsigned long *)&__m256d_result[3]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m256d_result[2]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0xfffffffffffffffc;
  *((unsigned long *)&__m256d_result[0]) = 0x3ff0000000000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0218ff78fc38fc38;
  *((unsigned long *)&__m256d_op0[2]) = 0xfc00000000000048;
  *((unsigned long *)&__m256d_op0[1]) = 0x0218ff78fc38fc38;
  *((unsigned long *)&__m256d_op0[0]) = 0xfc00000000000048;
  *((unsigned long *)&__m256d_result[3]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0xfc00000000000048;
  *((unsigned long *)&__m256d_result[1]) = 0x3ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0xfc00000000000048;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x8000800080008000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x8000800080008000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x8000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x8000000000000000;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_op0[2]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_op0[1]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_op0[0]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_result[3]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_result[2]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_result[1]) = 0xfffffff0fffffff0;
  *((unsigned long *)&__m256d_result[0]) = 0xfffffff0fffffff0;
  __m256d_out = __lasx_xvfrintrp_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x1e18000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x017e017e01dd61de;
  *((unsigned long *)&__m256d_op0[2]) = 0x5d637d043bc4fc43;
  *((unsigned long *)&__m256d_op0[1]) = 0x01dcc2dce31bc35d;
  *((unsigned long *)&__m256d_op0[0]) = 0x5e041d245b85fc43;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x5d637d043bc4fc43;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x5e041d245b85fc43;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_op0[2]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_op0[1]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_op0[0]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_result[3]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_result[2]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_result[1]) = 0x7c007c007c007c00;
  *((unsigned long *)&__m256d_result[0]) = 0x7c007c007c007c00;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000ffff00000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  unsigned_int_out = __lasx_xvpickve2gr_wu (__m256i_op0, 0x5);
  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrm_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_op0[0]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[3]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[2]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[1]) = 0xffffffffffffffff;
  *((unsigned long *)&__m256d_result[0]) = 0xffffffffffffffff;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000800000098;
  *((unsigned long *)&__m256d_op0[2]) = 0x000000040000ffca;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000800000098;
  *((unsigned long *)&__m256d_op0[0]) = 0x000000040000ff79;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_op0[0]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x7ff0000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x7ff0000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000064;
  *((unsigned long *)&__m256d_op0[1]) = 0x0000000000000781;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000064;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x000000001ffe2000;
  *((unsigned long *)&__m256d_op0[2]) = 0x001fe020001fe020;
  *((unsigned long *)&__m256d_op0[1]) = 0x000000001ffe2000;
  *((unsigned long *)&__m256d_op0[0]) = 0x001fe020001fe020;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  *((unsigned long *)&__m256d_op0[3]) = 0x0101010101010101;
  *((unsigned long *)&__m256d_op0[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_op0[1]) = 0x0101010101010101;
  *((unsigned long *)&__m256d_op0[0]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[3]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[2]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[1]) = 0x0000000000000000;
  *((unsigned long *)&__m256d_result[0]) = 0x0000000000000000;
  __m256d_out = __lasx_xvfrintrz_d (__m256d_op0);
  ASSERTEQ_64 (__LINE__, __m256d_result, __m256d_out);

  return 0;
}

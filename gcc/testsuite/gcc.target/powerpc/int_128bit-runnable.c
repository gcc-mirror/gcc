/* { dg-do run } */
/* { dg-options "-mcpu=power10 -save-temps" } */
/* { dg-require-effective-target power10_hw } */

/* Check that the expected 128-bit instructions are generated if the processor
   supports the 128-bit integer instructions. */
/* { dg-final { scan-assembler-times {\mvextsd2q\M} 6 } } */
/* { dg-final { scan-assembler-times {\mvslq\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsrq\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvsraq\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvrlq\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvrlqnm\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvrlqmi\M} 2 } } */
/* { dg-final { scan-assembler-times {\mvcmpequq\M} 16 } } */
/* { dg-final { scan-assembler-times {\mvcmpgtsq\M} 16 } } */
/* { dg-final { scan-assembler-times {\mvcmpgtuq\M} 16 } } */
/* { dg-final { scan-assembler-times {\mvmuloud\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulesd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulosd\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmulld\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivsq\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivuq\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdivesq\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvdiveuq\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmodsq\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvmoduq\M} 1 } } */

#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#include <stdlib.h>
#include <math.h>


void print_i128(__int128_t val)
{
  printf(" %lld %llu (0x%llx %llx)",
	 (signed long long)(val >> 64),
	 (unsigned long long)(val & 0xFFFFFFFFFFFFFFFF),
	 (unsigned long long)(val >> 64),
	 (unsigned long long)(val & 0xFFFFFFFFFFFFFFFF));
}
#endif

void abort (void);

__attribute__((noinline))
__int128_t shift_right (__int128_t a, __uint128_t b)
{
  return a >> b;
}

__attribute__((noinline))
__int128_t shift_left (__int128_t a, __uint128_t b)
{
  return a << b;
}

int main ()
{
  int i, result_int;

  __int128_t arg1, result;
  __uint128_t uarg2;

  _Decimal128 arg1_dfp128, result_dfp128, expected_result_dfp128;

  struct conv_t {
    __uint128_t u128;
    _Decimal128 d128;
  } conv, conv2;

  vector signed long long int vec_arg1_di, vec_arg2_di;
  vector signed long long int vec_result_di, vec_expected_result_di;
  vector unsigned long long int vec_uarg1_di, vec_uarg2_di, vec_uarg3_di;
  vector unsigned long long int vec_uresult_di;
  vector unsigned long long int vec_uexpected_result_di;
  
  __int128_t expected_result;
  __uint128_t uexpected_result;

  vector __int128 vec_arg1, vec_arg2, vec_result;
  vector unsigned __int128 vec_uarg1, vec_uarg2, vec_uarg3, vec_uresult;
  vector bool __int128  vec_result_bool;

  /* sign extend double to 128-bit integer  */
  vec_arg1_di[0] = 1000;
  vec_arg1_di[1] = -123456;

  expected_result = 1000;

  vec_result = vec_signextq (vec_arg1_di);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_signextq ((long long) %lld) =  ",  vec_arg1_di[0]);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1_di[0] = -123456;
  vec_arg1_di[1] = 1000;

  expected_result = -123456;

  vec_result = vec_signextq (vec_arg1_di);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_signextq ((long long) %lld) =  ",  vec_arg1_di[0]);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
  
  /* test shift 128-bit integers.
     Note, shift amount is given by the lower 7-bits of the shift amount. */
  vec_arg1[0] = 3;
  vec_uarg2[0] = 2;
  expected_result = vec_arg1[0]*4;

  vec_result = vec_sl (vec_arg1, vec_uarg2);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_sl(int128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" << %lld", vec_uarg2[0] & 0xFF);
    printf(" = ");
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  arg1 = vec_result[0];
  uarg2 = 4;
  expected_result = arg1*16;

  result = arg1 << uarg2;

  if (result != expected_result) {
#if DEBUG
    printf("ERROR: int128 << uint128):  ");
    print_i128(arg1);
    printf(" << %lld", uarg2 & 0xFF);
    printf(" = ");
    print_i128(result);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 3;
  vec_uarg2[0] = 2;
  uexpected_result = vec_uarg1[0]*4;
  
  vec_uresult = vec_sl (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_sl(uint128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" << %lld", vec_uarg2[0] & 0xFF);
    printf(" = ");
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12;
  vec_uarg2[0] = 2;
  expected_result = vec_arg1[0]/4;

  vec_result = vec_sr (vec_arg1, vec_uarg2);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_sr(int128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" >> %lld", vec_uarg2[0] & 0xFF);
    printf(" = ");
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 48;
  vec_uarg2[0] = 2;
  uexpected_result = vec_uarg1[0]/4;
  
  vec_uresult = vec_sr (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_sr(uint128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" >> %lld", vec_uarg2[0] & 0xFF);
    printf(" = ");
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  arg1 = vec_uresult[0];
  uarg2 = 4;
  expected_result = arg1/16;

  result = arg1 >> uarg2;

  if (result != expected_result) {
#if DEBUG
    printf("ERROR: int128 >> uint128:  ");
    print_i128(arg1);
    printf(" >> %lld", uarg2 & 0xFF);
    printf(" = ");
    print_i128(result);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 0x1234567890ABCDEFULL;
  vec_arg1[0] = (vec_arg1[0] << 64) | 0xAABBCCDDEEFF1122ULL;
  vec_uarg2[0] = 32;
  expected_result = 0x0000000012345678ULL;
  expected_result = (expected_result << 64) | 0x90ABCDEFAABBCCDDULL;

  vec_result = vec_sra (vec_arg1, vec_uarg2);
  
  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_sra(int128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" >> %lld = \n", vec_uarg2[0]);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 0xAABBCCDDEEFF1122ULL;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 0x1234567890ABCDEFULL;
  vec_uarg2[0] = 48;
  uexpected_result = 0xFFFFFFFFFFFFAABBLL;
  uexpected_result = (uexpected_result << 64) | 0xCCDDEEFF11221234ULL;

  vec_uresult = vec_sra (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_sra(uint128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" >> %lld = \n", vec_uarg2[0] & 0xFF);
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 0x1234567890ABCDEFULL;
  vec_arg1[0] = (vec_arg1[0] << 64) | 0xAABBCCDDEEFF1122ULL;
  vec_uarg2[0] = 32;
  expected_result = 0x90ABCDEFAABBCCDDULL;
  expected_result = (expected_result << 64) | 0xEEFF112212345678ULL;

  vec_result = vec_rl (vec_arg1, vec_uarg2);
  
  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_rl(int128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" >> %lld = \n", vec_uarg2[0]);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 0xAABBCCDDEEFF1122ULL;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 0x1234567890ABCDEFULL;
  vec_uarg2[0] = 48;
  uexpected_result = 0x11221234567890ABULL;
  uexpected_result = (uexpected_result << 64) | 0xCDEFAABBCCDDEEFFULL;

  vec_uresult = vec_rl (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_rl(uint128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" >> %lld = \n", vec_uarg2[0]);
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* vec_rlnm(arg1, arg2, arg3)
     result - rotate each element of arg1 left by shift in element of arg2.
       Then AND with mask whose  start/stop bits are specified in element of
       arg3.  */
  vec_arg1[0] = 0x1234567890ABCDEFULL;
  vec_arg1[0] = (vec_arg1[0] << 64) | 0xAABBCCDDEEFF1122ULL;
  vec_uarg2[0] = 32;
  vec_uarg3[0] = (32 << 8) | 95;
  expected_result = 0xaabbccddULL;
  expected_result = (expected_result << 64) | 0xeeff112200000000ULL;

  vec_result = vec_rlnm (vec_arg1, vec_uarg2, vec_uarg3);
  
  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_rlnm(int128, uint128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" << %lld = \n", vec_uarg3[0] & 0xFF);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  

  /* vec_rlnm(arg1, arg2, arg3)
     result - rotate each element of arg1 left by shift in element of arg2;
     then AND with mask whose  start/stop bits are specified in element of
     arg3.  */
  vec_uarg1[0] = 0xAABBCCDDEEFF1122ULL;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 0x1234567890ABCDEFULL;
  vec_uarg2[0] = 48;
  vec_uarg3[0] = (8 << 8) | 119;

  uexpected_result = 0x00221234567890ABULL;
  uexpected_result = (uexpected_result << 64) | 0xCDEFAABBCCDDEE00ULL;

  vec_uresult = vec_rlnm (vec_uarg1, vec_uarg2, vec_uarg3);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_rlnm(uint128, uint128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" << %lld = \n", vec_uarg3[0] & 0xFF);
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /*  vec_rlmi(R, A, B)
      Result value: Each element of R is obtained by rotating the corresponding
      element of A left by the number of bits specified by the corresponding element
      of B.  */

  vec_arg1[0] = 0x1234567890ABCDEFULL;
  vec_arg1[0] = (vec_arg1[0] << 64) | 0xAABBCCDDEEFF1122ULL;
  vec_arg2[0] = 0x000000000000DEADULL;
  vec_arg2[0] = (vec_arg2[0] << 64) | 0x0000BEEF00000000ULL;
  vec_uarg3[0] = 96 << 16 | 127 << 8 | 32;
  expected_result = 0x000000000000DEADULL;
  expected_result = (expected_result << 64) | 0x0000BEEF12345678ULL;

  vec_result = vec_rlmi (vec_arg1, vec_arg2, vec_uarg3);
  
  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_rlmi(int128, int128, uint128):  ");
    print_i128(vec_arg1[0]);
    printf(" << %lld = \n", vec_uarg2_di[1] & 0xFF);
    print_i128(vec_result[0]);
    printf("\n does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* vec_rlmi(R, A, B)
     Result value: Each element of R is obtained by rotating the corresponding
     element of A left by the number of bits specified by the corresponding element
     of B.  */

  vec_uarg1[0] = 0xAABBCCDDEEFF1122ULL;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 0x1234567890ABCDEFULL;
  vec_uarg2[0] = 0xDEAD000000000000ULL;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 0x000000000000BEEFULL;
  vec_uarg3[0] = 16 << 16 | 111 << 8 | 48;
  uexpected_result = 0xDEAD1234567890ABULL;
  uexpected_result = (uexpected_result << 64) | 0xCDEFAABBCCDDBEEFULL;

  vec_uresult = vec_rlmi (vec_uarg1, vec_uarg2, vec_uarg3);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_rlmi(uint128, unit128, uint128):  ");
    print_i128(vec_uarg1[0]);
    printf(" << %lld = \n", vec_uarg3[1] & 0xFF);
    print_i128(vec_uresult[0]);
    printf("\n does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* 128-bit compare tests, result is all 1's if true */
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1[0] = 2468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  uexpected_result = 0xFFFFFFFFFFFFFFFFULL;
  uexpected_result = (uexpected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpgt (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: unsigned vec_cmpgt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpgt (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed vec_cmpgt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }


  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = -1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmpeq (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR:not equal signed vec_cmpeq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpeq (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed equal vec_cmpeq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 12468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmpeq (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  not equal vec_cmpeq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpeq (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: equal unsigned vec_cmpeq ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 12468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpne (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  not equal vec_cmpne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmpne (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: equal unsigned vec_cmpne ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = -1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpne (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR:not equal signed vec_cmpne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmpne (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed equal vec_cmpne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 12468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmplt (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 > arg2 vec_cmplt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 1234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 12468;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmplt (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 < arg2 vec_cmplt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmplt (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR:  unsigned arg1 = arg2 vec_cmplt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = -1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmplt (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 > arg2 vec_cmplt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = -1234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 12468;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmplt (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 < arg2 vec_cmplt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;
  expected_result = 0x0ULL;

  vec_result_bool = vec_cmplt (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_cmplt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
   
  vec_uarg1[0] = 12468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmple (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 > arg2 vec_cmple ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 1234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 12468;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmple (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 < arg2 vec_cmple ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmple (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR:  unsigned arg1 = arg2 vec_cmple ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = -1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmple (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 > arg2 vec_cmple ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = -1234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 12468;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmple (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 < arg2 vec_cmple ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmple (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_cmple ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 12468;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpge (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 > arg2 vec_cmpge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 1234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 12468;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmpge (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: unsigned  arg1 < arg2 vec_cmpge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpge (vec_uarg1, vec_uarg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR:  unsigned arg1 = arg2 vec_cmpge ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = 12468;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = -1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpge (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 > arg2 vec_cmpge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg1[0] = -1234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 12468;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  expected_result = 0x0;

  vec_result_bool = vec_cmpge (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed  arg1 < arg2 vec_cmpge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;
  expected_result = 0xFFFFFFFFFFFFFFFFULL;
  expected_result = (expected_result << 64) | 0xFFFFFFFFFFFFFFFFULL;

  vec_result_bool = vec_cmpge (vec_arg1, vec_arg2);

  if (vec_result_bool[0] != expected_result) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_cmpge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.");
    print_i128(vec_result_bool[0]);
    printf("\n Result does not match expected_result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

#if 1
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_eq (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_eq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_eq (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_eq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_eq (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_eq ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_eq (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_eq ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_ne (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_ne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_ne (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_ne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_ne (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_ne ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_ne (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_ne ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_lt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_lt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_lt (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_lt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_lt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_lt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_lt (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_lt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_le (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_le ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_le (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_le ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_le (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_le ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_le (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_le ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_gt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_gt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_gt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_gt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_gt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_gt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_gt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_gt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_all_ge (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_all_ge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_all_ge (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_all_ge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_all_ge (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_all_ge ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_all_ge (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_all_ge ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_eq (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_eq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_eq (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_eq ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_eq (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_eq ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_eq (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_eq ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_ne (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_ne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_ne (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_ne ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_ne (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_ne ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_ne (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_ne ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_lt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_lt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_lt (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_lt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_lt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_lt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_lt (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_lt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_gt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_gt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_gt (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_gt ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_gt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_gt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_gt (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_gt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_le (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_le ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_le (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_le ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_le (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_le ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_le (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_le ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }

  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;
  vec_arg1 = vec_arg2;

  result_int = vec_any_ge (vec_arg1, vec_arg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: signed arg1 = arg2 vec_any_ge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1[0] = -234;
  vec_arg1[0] = (vec_arg1[0] << 64) | 4567;
  vec_arg2[0] = 1234;
  vec_arg2[0] = (vec_arg2[0] << 64) | 4567;

  result_int = vec_any_ge (vec_arg1, vec_arg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: signed arg1 != arg2 vec_any_ge ( ");
    print_i128(vec_arg1[0]);
    printf(", ");
    print_i128(vec_arg2[0]);
    printf(") failed.\n\n");
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;
  vec_uarg1 = vec_uarg2;

  result_int = vec_any_ge (vec_uarg1, vec_uarg2);

  if (!result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 = uarg2 vec_any_ge ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1[0] = 234;
  vec_uarg1[0] = (vec_uarg1[0] << 64) | 4567;
  vec_uarg2[0] = 1234;
  vec_uarg2[0] = (vec_uarg2[0] << 64) | 4567;

  result_int = vec_any_ge (vec_uarg1, vec_uarg2);

  if (result_int) {
#if DEBUG
    printf("ERROR: unsigned uarg1 != uarg2 vec_any_gt ( ");
    print_i128(vec_uarg1[0]);
    printf(", ");
    print_i128(vec_uarg2[0]);
    printf(") failed.\n\n");
#else
    abort();
#endif
  }
#endif

  /* Vector multiply Even and Odd tests */
  vec_arg1_di[0] = 200;
  vec_arg1_di[1] = 400;
  vec_arg2_di[0] = 1234;
  vec_arg2_di[1] = 4567;
  expected_result = vec_arg1_di[0] * vec_arg2_di[0];

  vec_result = vec_mule (vec_arg1_di, vec_arg2_di);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_mule (signed, signed) failed.\n");
    printf(" vec_arg1_di[0] = %lld\n", vec_arg1_di[0]);
    printf(" vec_arg2_di[0] = %lld\n", vec_arg2_di[0]);
    printf("Result = ");
    print_i128(vec_result[0]);
    printf("\nExpected Result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_arg1_di[0] = -200;
  vec_arg1_di[1] = -400;
  vec_arg2_di[0] = 1234;
  vec_arg2_di[1] = 4567;
  expected_result = vec_arg1_di[1] * vec_arg2_di[1];

  vec_result = vec_mulo (vec_arg1_di, vec_arg2_di);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_mulo (signed, signed) failed.\n");
    printf(" vec_arg1_di[1] = %lld\n", vec_arg1_di[1]);
    printf(" vec_arg2_di[1] = %lld\n", vec_arg2_di[1]);
    printf("Result = ");
    print_i128(vec_result[0]);
    printf("\nExpected Result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1_di[0] = 200;
  vec_uarg1_di[1] = 400;
  vec_uarg2_di[0] = 1234;
  vec_uarg2_di[1] = 4567;
  uexpected_result = vec_uarg1_di[0] * vec_uarg2_di[0];

  vec_uresult = vec_mule (vec_uarg1_di, vec_uarg2_di);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_mule (unsigned, unsigned) failed.\n");
    printf(" vec_uarg1_di[1] = %lld\n", vec_uarg1_di[1]);
    printf(" vec_uarg2_di[1] = %lld\n", vec_uarg2_di[1]);
    printf("Result = ");
    print_i128(vec_uresult[0]);
    printf("\nExpected Result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
  
  vec_uarg1_di[0] = 200;
  vec_uarg1_di[1] = 400;
  vec_uarg2_di[0] = 1234;
  vec_uarg2_di[1] = 4567;
  uexpected_result = vec_uarg1_di[1] * vec_uarg2_di[1];

  vec_uresult = vec_mulo (vec_uarg1_di, vec_uarg2_di);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_mulo (unsigned, unsigned) failed.\n");
    printf(" vec_uarg1_di[0] = %lld\n", vec_uarg1_di[0]);
    printf(" vec_uarg2_di[0] = %lld\n", vec_uarg2_di[0]);
    printf("Result = ");
    print_i128(vec_uresult[0]);
    printf("\nExpected Result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* Vector Multiply Longword */
  vec_arg1_di[0] = 100;
  vec_arg1_di[1] = -123456;

  vec_arg2_di[0] = 123;
  vec_arg2_di[1] = 1000;

  vec_expected_result_di[0] = 12300;
  vec_expected_result_di[1] = -123456000;

  vec_result_di = vec_arg1_di * vec_arg2_di;

  for (i = 0; i<2; i++) {
    if (vec_result_di[i] != vec_expected_result_di[i]) {
#if DEBUG
      printf("ERROR: vector multipy [%d] ((long long) %lld) =  ", i,
	     vec_result_di[i]);
      printf("\n does not match expected_result [%d] = ((long long) %lld)", i,
	     vec_expected_result_di[i]);
      printf("\n\n");
#else
      abort();
#endif
    }
  }

  /* Vector Divide Quadword */
  vec_arg1[0] = -12345678;
  vec_arg2[0] = 2;
  expected_result = -6172839;

  vec_result = vec_div (vec_arg1, vec_arg2);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_div (signed, signed) failed.\n");
    printf("vec_arg1[0] = ");
    print_i128(vec_arg1[0]);
    printf("\nvec_arg2[0] = ");
    print_i128(vec_arg2[0]);
    printf("\nResult = ");
    print_i128(vec_result[0]);
    printf("\nExpected result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 24680;
  vec_uarg2[0] = 4;
  uexpected_result = 6170;

  vec_uresult = vec_div (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_div (unsigned, unsigned) failed.\n");
    printf("vec_uarg1[0] = ");
    print_i128(vec_uarg1[0]);
    printf("\nvec_uarg2[0] = ");
    print_i128(vec_uarg2[0]);
    printf("\nResult = ");
    print_i128(vec_uresult[0]);
    printf("\nExpected result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* Vector Divide Extended Quadword */
  vec_arg1[0] = -20;        // has 128-bit of zero concatenated onto it
  vec_arg2[0] = 0x2000000000000000;
  vec_arg2[0] = vec_arg2[0] << 64;
  expected_result = -160;

  vec_result = vec_dive (vec_arg1, vec_arg2);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_dive (signed, signed) failed.\n");
    printf("vec_arg1[0] = ");
    print_i128(vec_arg1[0]);
    printf("\nvec_arg2[0] = ");
    print_i128(vec_arg2[0]);
    printf("\nResult = ");
    print_i128(vec_result[0]);
    printf("\nExpected result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 20;        // has 128-bit of zero concatenated onto it
  vec_uarg2[0] = 0x4000000000000000;
  vec_uarg2[0] = vec_uarg2[0] << 64;
  uexpected_result = 80;

  vec_uresult = vec_dive (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_dive (unsigned, unsigned) failed.\n");
    printf("vec_uarg1[0] = ");
    print_i128(vec_uarg1[0]);
    printf("\nvec_uarg2[0] = ");
    print_i128(vec_uarg2[0]);
    printf("\nResult = ");
    print_i128(vec_uresult[0]);
    printf("\nExpected result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  /* Vector modulo quad word  */
  vec_arg1[0] = -12345675;
  vec_arg2[0] = 2;
  expected_result = -1;

  vec_result = vec_mod (vec_arg1, vec_arg2);

  if (vec_result[0] != expected_result) {
#if DEBUG
    printf("ERROR: vec_mod (signed, signed) failed.\n");
    printf("vec_arg1[0] = ");
    print_i128(vec_arg1[0]);
    printf("\nvec_arg2[0] = ");
    print_i128(vec_arg2[0]);
    printf("\nResult = ");
    print_i128(vec_result[0]);
    printf("\nExpected result = ");
    print_i128(expected_result);
    printf("\n\n");
#else
    abort();
#endif
  }

  vec_uarg1[0] = 24685;
  vec_uarg2[0] = 4;
  uexpected_result = 1;

  vec_uresult = vec_mod (vec_uarg1, vec_uarg2);

  if (vec_uresult[0] != uexpected_result) {
#if DEBUG
    printf("ERROR: vec_mod (unsigned, unsigned) failed.\n");
    printf("vec_uarg1[0] = ");
    print_i128(vec_uarg1[0]);
    printf("\nvec_uarg2[0] = ");
    print_i128(vec_uarg2[0]);
    printf("\nResult = ");
    print_i128(vec_uresult[0]);
    printf("\nExpected result = ");
    print_i128(uexpected_result);
    printf("\n\n");
#else
    abort();
#endif
  }
  
  /* DFP to __int128 and __int128 to DFP conversions */
  /* Print the DFP value as an unsigned int so we can see the bit patterns.  */
  conv.u128 = 0x2208000000000000ULL;
  conv.u128 = (conv.u128 << 64) | 0x4ULL;   //DFP bit pattern for integer 4
  expected_result_dfp128 = conv.d128;

  arg1 = 4;

  conv.d128 = (_Decimal128) arg1;

  result_dfp128 = (_Decimal128) arg1;
  if (((conv.u128 >>64) != 0x2208000000000000ULL) &&
      ((conv.u128 & 0xFFFFFFFFFFFFFFFF) != 0x4ULL)) {
#if DEBUG
    printf("ERROR:  convert int128 value ");
    print_i128 (arg1);
    conv.d128 = result_dfp128;
    printf("\nto DFP value 0x%llx %llx (printed as hex bit string) ",
	   (unsigned long long)((conv.u128) >>64),
	   (unsigned long long)((conv.u128) & 0xFFFFFFFFFFFFFFFF));

    conv.d128 = expected_result_dfp128;
    printf("\ndoes not match expected_result = 0x%llx %llx\n\n",
	   (unsigned long long) (conv.u128>>64),
	   (unsigned long long) (conv.u128 & 0xFFFFFFFFFFFFFFFF));
#else
    abort();
#endif
  }

  expected_result = 4;

  conv.u128 = 0x2208000000000000ULL;
  conv.u128 = (conv.u128 << 64) | 0x4ULL;  // 4 as DFP
  arg1_dfp128 = conv.d128;

  result = (__int128_t) arg1_dfp128;

  if (result != expected_result) {
#if DEBUG
    printf("ERROR:  convert DFP value ");
    printf("0x%llx %llx (printed as hex bit string) ",
	   (unsigned long long)(conv.u128>>64),
	   (unsigned long long)(conv.u128 & 0xFFFFFFFFFFFFFFFF));
    printf("to __int128 value = ");
    print_i128 (result);
    printf("\ndoes not match expected_result = ");
    print_i128 (expected_result);
    printf("\n");
#else
    abort();
#endif
  }
  return 0;
}

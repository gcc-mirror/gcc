#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xde, 0xbc, 0x9a,
				       0x78, 0x56, 0x34, 0x12 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xdef0, 0x9abc, 0x5678, 0x1234 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x9abcdef0, 0x12345678 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x123456789abcdef0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xde, 0xbc, 0x9a,
					0x78, 0x56, 0x34, 0x12 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xdef0, 0x9abc, 0x5678, 0x1234 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x9abcdef0, 0x12345678 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x123456789abcdef0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf0, 0xde, 0xbc, 0x9a,
					0x78, 0x56, 0x34, 0x12 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xdef0, 0x9abc, 0x5678, 0x1234 };
VECT_VAR_DECL(expected,hfloat,16,4) [] = { 0xdef0, 0x9abc, 0x5678, 0x1234 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0x9abcdef0, 0x12345678 };

#define INSN_NAME vcreate
#define TEST_MSG "VCREATE"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=vcreate(x), then store the result.  */
#define TEST_VCREATE(T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) = vcreate_##T2##W(VECT_VAR(val, T1, W, N)); \
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

#define DECL_VAL(VAR, T1, W, N)			\
  uint64_t VECT_VAR(VAR, T1, W, N)

  DECL_VAL(val, int, 8, 8);
  DECL_VAL(val, int, 16, 4);
  DECL_VAL(val, int, 32, 2);
  DECL_VAL(val, int, 64, 1);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  DECL_VAL(val, float, 16, 4);
#endif
  DECL_VAL(val, float, 32, 2);
  DECL_VAL(val, uint, 8, 8);
  DECL_VAL(val, uint, 16, 4);
  DECL_VAL(val, uint, 32, 2);
  DECL_VAL(val, uint, 64, 1);
  DECL_VAL(val, poly, 8, 8);
  DECL_VAL(val, poly, 16, 4);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 64, 1);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  DECL_VARIABLE(vector_res, float, 16, 4);
#endif
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);
  DECL_VARIABLE(vector_res, uint, 64, 1);
  DECL_VARIABLE(vector_res, poly, 8, 8);
  DECL_VARIABLE(vector_res, poly, 16, 4);

  clean_results ();

  /* Initialize input values arbitrarily.  */
  VECT_VAR(val, int, 8, 8) = 0x123456789abcdef0LL;
  VECT_VAR(val, int, 16, 4) = 0x123456789abcdef0LL;
  VECT_VAR(val, int, 32, 2) = 0x123456789abcdef0LL;
  VECT_VAR(val, int, 64, 1) = 0x123456789abcdef0LL;
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VECT_VAR(val, float, 16, 4) = 0x123456789abcdef0LL;
#endif
  VECT_VAR(val, float, 32, 2) = 0x123456789abcdef0LL;
  VECT_VAR(val, uint, 8, 8) = 0x123456789abcdef0ULL;
  VECT_VAR(val, uint, 16, 4) = 0x123456789abcdef0ULL;
  VECT_VAR(val, uint, 32, 2) = 0x123456789abcdef0ULL;
  VECT_VAR(val, uint, 64, 1) = 0x123456789abcdef0ULL;
  VECT_VAR(val, poly, 8, 8) = 0x123456789abcdef0ULL;
  VECT_VAR(val, poly, 16, 4) = 0x123456789abcdef0ULL;

  TEST_VCREATE(int, s, 8, 8);
  TEST_VCREATE(int, s, 16, 4);
  TEST_VCREATE(int, s, 32, 2);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VCREATE(float, f, 16, 4);
#endif
  TEST_VCREATE(float, f, 32, 2);
  TEST_VCREATE(int, s, 64, 1);
  TEST_VCREATE(uint, u, 8, 8);
  TEST_VCREATE(uint, u, 16, 4);
  TEST_VCREATE(uint, u, 32, 2);
  TEST_VCREATE(uint, u, 64, 1);
  TEST_VCREATE(poly, p, 8, 8);
  TEST_VCREATE(poly, p, 16, 4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected, "");
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
}

int main (void)
{
  exec_vcreate ();
  return 0;
}

#define INSN_NAME vneg
#define TEST_MSG "VNEG/VNEGQ"

/* Extra tests for functions requiring floating-point types.  */
void exec_vneg_f32(void);
#define EXTRA_TESTS exec_vneg_f32

#include "unary_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x10, 0xf, 0xe, 0xd,
				       0xc, 0xb, 0xa, 0x9 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x10, 0xf, 0xe, 0xd };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x10, 0xf };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x10, 0xf, 0xe, 0xd, 0xc, 0xb, 0xa, 0x9,
					0x8, 0x7, 0x6, 0x5, 0x4, 0x3, 0x2, 0x1 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x10, 0xf, 0xe, 0xd,
					0xc, 0xb, 0xa, 0x9 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x10, 0xf, 0xe, 0xd };

/* Expected results for float32 variants. Needs to be separated since
   the generic test function does not test floating-point
   versions.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_float16, hfloat, 16, 4) [] = { 0xc09a, 0xc09a,
						      0xc09a, 0xc09a };
VECT_VAR_DECL(expected_float16, hfloat, 16, 8) [] = { 0xc2cd, 0xc2cd,
						      0xc2cd, 0xc2cd,
						      0xc2cd, 0xc2cd,
						      0xc2cd, 0xc2cd };
#endif
VECT_VAR_DECL(expected_float32,hfloat,32,2) [] = { 0xc0133333, 0xc0133333 };
VECT_VAR_DECL(expected_float32,hfloat,32,4) [] = { 0xc059999a, 0xc059999a,
						   0xc059999a, 0xc059999a };

void exec_vneg_f32(void)
{
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);
#endif
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);


#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);
#endif
  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 2.3f);
  VDUP(vector, q, float, f, 16, 8, 3.4f);
#endif
  VDUP(vector, , float, f, 32, 2, 2.3f);
  VDUP(vector, q, float, f, 32, 4, 3.4f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  TEST_UNARY_OP(INSN_NAME, , float, f, 16, 4);
  TEST_UNARY_OP(INSN_NAME, q, float, f, 16, 8);
#endif
  TEST_UNARY_OP(INSN_NAME, , float, f, 32, 2);
  TEST_UNARY_OP(INSN_NAME, q, float, f, 32, 4);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_float16, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_float16, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_float32, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_float32, "");
}

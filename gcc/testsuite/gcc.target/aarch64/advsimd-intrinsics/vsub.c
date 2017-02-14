#define INSN_NAME vsub
#define TEST_MSG "VSUB/VSUBQ"

/* Extra tests for functions requiring floating-point types */
void exec_vsub_f32(void);
#define EXTRA_TESTS exec_vsub_f32

#include "binary_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xee, 0xef, 0xf0, 0xf1,
				       0xf2, 0xf3, 0xf4, 0xf5 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffed, 0xffffffee };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffff8c };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xdc, 0xdd, 0xde, 0xdf,
					0xe0, 0xe1, 0xe2, 0xe3 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffd2, 0xffd3, 0xffd4, 0xffd5 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffc8, 0xffffffc9 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffffee };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xfa, 0xfb, 0xfc, 0xfd,
					0xfe, 0xff, 0x0, 0x1,
					0x2, 0x3, 0x4, 0x5,
					0x6, 0x7, 0x8, 0x9 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x4, 0x5, 0x6, 0x7,
					0x8, 0x9, 0xa, 0xb };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xe, 0xf, 0x10, 0x11 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffd8,
					0xffffffffffffffd9 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xe4, 0xe5, 0xe6, 0xe7,
					 0xe8, 0xe9, 0xea, 0xeb,
					 0xec, 0xed, 0xee, 0xef,
					 0xf0, 0xf1, 0xf2, 0xf3};
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xffed, 0xffee, 0xffef, 0xfff0,
					 0xfff1, 0xfff2, 0xfff3, 0xfff4 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffb9, 0xffffffba,
					 0xffffffbb, 0xffffffbc };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffffed,
					 0xffffffffffffffee };

/* Expected results for float32 variants. Needs to be separated since
   the generic test function does not test floating-point
   versions.  */
VECT_VAR_DECL(expected_float32,hfloat,32,2) [] = { 0xc00ccccd, 0xc00ccccd };
VECT_VAR_DECL(expected_float32,hfloat,32,4) [] = { 0xc00ccccc, 0xc00ccccc,
						   0xc00ccccc, 0xc00ccccc };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_float16, hfloat, 16, 4) [] = { 0xc066, 0xc066,
						      0xc066, 0xc066 };
VECT_VAR_DECL(expected_float16, hfloat, 16, 8) [] = { 0xc067, 0xc067,
						      0xc067, 0xc067,
						      0xc067, 0xc067,
						      0xc067, 0xc067 };
#endif

void exec_vsub_f32(void)
{
  DECL_VARIABLE(vector, float, 32, 2);
  DECL_VARIABLE(vector, float, 32, 4);

  DECL_VARIABLE(vector2, float, 32, 2);
  DECL_VARIABLE(vector2, float, 32, 4);

  DECL_VARIABLE(vector_res, float, 32, 2);
  DECL_VARIABLE(vector_res, float, 32, 4);

  VDUP(vector, , float, f, 32, 2, 2.3f);
  VDUP(vector, q, float, f, 32, 4, 3.4f);

  VDUP(vector2, , float, f, 32, 2, 4.5f);
  VDUP(vector2, q, float, f, 32, 4, 5.6f);

  TEST_BINARY_OP(INSN_NAME, , float, f, 32, 2);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 32, 4);

  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected_float32, "");
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected_float32, "");

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  DECL_VARIABLE(vector, float, 16, 4);
  DECL_VARIABLE(vector, float, 16, 8);

  DECL_VARIABLE(vector2, float, 16, 4);
  DECL_VARIABLE(vector2, float, 16, 8);

  DECL_VARIABLE(vector_res, float, 16, 4);
  DECL_VARIABLE(vector_res, float, 16, 8);

  VDUP(vector, , float, f, 16, 4, 2.3f);
  VDUP(vector, q, float, f, 16, 8, 3.4f);

  VDUP(vector2, , float, f, 16, 4, 4.5f);
  VDUP(vector2, q, float, f, 16, 8, 5.6f);

  TEST_BINARY_OP(INSN_NAME, , float, f, 16, 4);
  TEST_BINARY_OP(INSN_NAME, q, float, f, 16, 8);

  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected_float16, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected_float16, "");
#endif
}

#define INSN_NAME vceq
#define TEST_MSG "VCEQ/VCEQQ"

/* Extra tests for _p8 variants, which exist only for vceq.  */
void exec_vceq_p8(void);
#define EXTRA_TESTS exec_vceq_p8

#include "cmp_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0xff, 0x0 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x0, 0x0, 0xffff, 0x0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0x0 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					 0x0, 0x0, 0x0, 0x0,
					 0x0, 0x0, 0x0, 0x0,
					 0xff, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					 0x0, 0x0, 0xffff, 0x0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x0, 0xffffffff, 0x0 };

VECT_VAR_DECL(expected_uint,uint,8,8) [] = { 0x0, 0x0, 0x0, 0xff,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_uint,uint,16,4) [] = { 0x0, 0x0, 0xffff, 0x0 };
VECT_VAR_DECL(expected_uint,uint,32,2) [] = { 0x0, 0xffffffff };

VECT_VAR_DECL(expected_q_uint,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						0xff, 0x0, 0x0, 0x0,
						0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_q_uint,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0xffff, 0x0 };
VECT_VAR_DECL(expected_q_uint,uint,32,4) [] = { 0x0, 0x0, 0xffffffff, 0x0 };

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected_float, uint, 16, 4) [] = { 0x0, 0xffff, 0x0, 0x0 };
VECT_VAR_DECL (expected_q_float, uint, 16, 8) [] = { 0x0, 0x0, 0xffff, 0x0,
						     0x0, 0x0, 0x0, 0x0, };
#endif

VECT_VAR_DECL(expected_float,uint,32,2) [] = { 0x0, 0xffffffff };
VECT_VAR_DECL(expected_q_float,uint,32,4) [] = { 0x0, 0x0, 0xffffffff, 0x0 };

VECT_VAR_DECL(expected_uint2,uint,32,2) [] = { 0xffffffff, 0x0 };
VECT_VAR_DECL(expected_uint3,uint,32,2) [] = { 0x0, 0xffffffff };
VECT_VAR_DECL(expected_uint4,uint,32,2) [] = { 0xffffffff, 0x0 };

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL (expected_nan, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0  };
VECT_VAR_DECL (expected_mnan, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_nan2, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };

VECT_VAR_DECL (expected_inf, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_minf, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_inf2, uint, 16, 4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL (expected_mzero, uint, 16, 4) [] = { 0xffff, 0xffff,
						   0xffff, 0xffff };
#endif

VECT_VAR_DECL(expected_nan,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_mnan,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_nan2,uint,32,2) [] = { 0x0, 0x0 };

VECT_VAR_DECL(expected_inf,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_minf,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_inf2,uint,32,2) [] = { 0x0, 0x0 };

VECT_VAR_DECL(expected_mzero,uint,32,2) [] = { 0xffffffff, 0xffffffff };

VECT_VAR_DECL(expected_p8,uint,8,8) [] = { 0x0, 0x0, 0x0, 0xff,
					   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_q_p8,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
					      0xff, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };

void exec_vceq_p8(void)
{
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector, poly, 8, 16);

  DECL_VARIABLE(vector2, poly, 8, 8);
  DECL_VARIABLE(vector2, poly, 8, 16);

  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 8, 16);

  clean_results ();

  VLOAD(vector, buffer, , poly, p, 8, 8);
  VLOAD(vector, buffer, q, poly, p, 8, 16);

  VDUP(vector2, , poly, p, 8, 8, 0xF3);
  VDUP(vector2, q, poly, p, 8, 16, 0xF4);

  TEST_VCOMP(INSN_NAME, , poly, p, uint, 8, 8);
  TEST_VCOMP(INSN_NAME, q, poly, p, uint, 8, 16);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_p8, "p8");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_q_p8, "p8");
}

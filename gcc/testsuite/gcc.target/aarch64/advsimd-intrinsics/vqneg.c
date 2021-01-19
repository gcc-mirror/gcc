#define INSN_NAME vqneg
#define TEST_MSG "VQNEG/VQNEGQ"

/* Extra tests for functions requiring corner cases tests */
void vqneg_extra(void);
#define EXTRA_TESTS vqneg_extra

#include "unary_sat_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x10, 0xf, 0xe, 0xd, 0xc, 0xb, 0xa, 0x9 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x10, 0xf, 0xe, 0xd };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x10, 0xf };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x10, 0xf, 0xe, 0xd,
					0xc, 0xb, 0xa, 0x9,
					0x8, 0x7, 0x6, 0x5,
					0x4, 0x3, 0x2, 0x1 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x10, 0xf, 0xe, 0xd,
					0xc, 0xb, 0xa, 0x9 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x10, 0xf, 0xe, 0xd };

/* Expected results when input is the min negative value of the type.  */
VECT_VAR_DECL(expected_min_neg,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					       0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_min_neg,int,16,4) [] = { 0x7fff, 0x7fff,
						0x7fff, 0x7fff };
VECT_VAR_DECL(expected_min_neg,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_min_neg,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_min_neg,int,16,8) [] = { 0x7fff, 0x7fff,
						0x7fff, 0x7fff,
						0x7fff, 0x7fff,
						0x7fff, 0x7fff };
VECT_VAR_DECL(expected_min_neg,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
						0x7fffffff, 0x7fffffff };

void vqneg_extra()
{
  /* No need for 64 bits variants.  */
  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);

  clean_results ();

  /* Initialize input "vector" with min negative values to check
     saturation.  */
  VDUP(vector, , int, s, 8, 8, 0x80);
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, q, int, s, 8, 16, 0x80);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);

#define MSG "min negative input"
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 8, 8, MSG);
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 16, 4, MSG);
  TEST_UNARY_SAT_OP(INSN_NAME, , int, s, 32, 2, MSG);
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 8, 16, MSG);
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 16, 8, MSG);
  TEST_UNARY_SAT_OP(INSN_NAME, q, int, s, 32, 4, MSG);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_min_neg, MSG);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_min_neg, MSG);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_min_neg, MSG);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_min_neg, MSG);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_min_neg, MSG);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_min_neg, MSG);
}

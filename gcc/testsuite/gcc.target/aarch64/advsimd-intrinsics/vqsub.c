#define INSN_NAME vqsub
#define TEST_MSG "VQSUB/VQSUBQ"

/* Extra tests for special cases:
   - some requiring intermediate types larger than 64 bits to
   compute saturation flag.
   - corner case saturations with types smaller than 64 bits.
*/
void vqsub_extras(void);
#define EXTRA_TESTS vqsub_extras

#include "binary_sat_op.inc"


/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xdf, 0xe0, 0xe1, 0xe2,
				       0xe3, 0xe4, 0xe5, 0xe6 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffce, 0xffcf,
					0xffd0, 0xffd1 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffbd, 0xffffffbe };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffac };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x9b, 0x9c, 0x9d, 0x9e,
					0x9f, 0xa0, 0xa1, 0xa2 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xff8a, 0xff8b,
					 0xff8c, 0xff8d };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffff79, 0xffffff7a };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffff68 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xdf, 0xe0, 0xe1, 0xe2,
					0xe3, 0xe4, 0xe5, 0xe6,
					0xe7, 0xe8, 0xe9, 0xea,
					0xeb, 0xec, 0xed, 0xee };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffce, 0xffcf, 0xffd0, 0xffd1,
					0xffd2, 0xffd3, 0xffd4, 0xffd5 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffbd, 0xffffffbe,
					0xffffffbf, 0xffffffc0 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffac,
					0xffffffffffffffad };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x9b, 0x9c, 0x9d, 0x9e,
					 0x9f, 0xa0, 0xa1, 0xa2,
					 0xa3, 0xa4, 0xa5, 0xa6,
					 0xa7, 0xa8, 0xa9, 0xaa };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xff8a, 0xff8b, 0xff8c, 0xff8d,
					 0xff8e, 0xff8f, 0xff90, 0xff91 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffff79, 0xffffff7a,
					 0xffffff7b, 0xffffff7c };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffff68,
					 0xffffffffffffff69 };

/* Expected values of cumulative saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,1) = 0;
int VECT_VAR(expected_cumulative_sat,uint,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,uint,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,uint,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,uint,64,1) = 0;
int VECT_VAR(expected_cumulative_sat,int,8,16) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat,uint,8,16) = 0;
int VECT_VAR(expected_cumulative_sat,uint,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,uint,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,uint,64,2) = 0;

/* 64-bits types, with 0 as second input.  */
VECT_VAR_DECL(expected_64,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_64,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_64,int,64,2) [] = { 0xfffffffffffffff0,
					   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_64,uint,64,2) [] = { 0xfffffffffffffff0,
					    0xfffffffffffffff1 };
int VECT_VAR(expected_cumulative_sat_64,int,64,1) = 0;
int VECT_VAR(expected_cumulative_sat_64,uint,64,1) = 0;
int VECT_VAR(expected_cumulative_sat_64,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat_64,uint,64,2) = 0;

/* 64-bits types, other cases.  */
VECT_VAR_DECL(expected_64_2,int,64,1) [] = { 0xffffffffffffffac };
VECT_VAR_DECL(expected_64_2,uint,64,1) [] = { 0xffffffffffffff68 };
VECT_VAR_DECL(expected_64_2,int,64,2) [] = { 0xffffffffffffffac,
					     0xffffffffffffffad };
VECT_VAR_DECL(expected_64_2,uint,64,2) [] = { 0xffffffffffffff68,
					      0xffffffffffffff69 };
int VECT_VAR(expected_cumulative_sat_64_2,int,64,1) = 0;
int VECT_VAR(expected_cumulative_sat_64_2,uint,64,1) = 0;
int VECT_VAR(expected_cumulative_sat_64_2,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat_64_2,uint,64,2) = 0;

/* 64-bits types, all causing cumulative saturation.  */
VECT_VAR_DECL(expected_64_3,int,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_64_3,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_64_3,int,64,2) [] = { 0x7fffffffffffffff,
					     0x7fffffffffffffff };
VECT_VAR_DECL(expected_64_3,uint,64,2) [] = { 0x0, 0x0 };
int VECT_VAR(expected_cumulative_sat_64_3,int,64,1) = 1;
int VECT_VAR(expected_cumulative_sat_64_3,uint,64,1) = 1;
int VECT_VAR(expected_cumulative_sat_64_3,int,64,2) = 1;
int VECT_VAR(expected_cumulative_sat_64_3,uint,64,2) = 1;

/* smaller types, corner cases causing cumulative saturation. (1)  */
VECT_VAR_DECL(expected_lt_64_1,int,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
					       0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_lt_64_1,int,16,4) [] = { 0x8000, 0x8000,
						0x8000, 0x8000 };
VECT_VAR_DECL(expected_lt_64_1,int,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_lt_64_1,int,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_lt_64_1,int,16,8) [] = { 0x8000, 0x8000,
						0x8000, 0x8000,
						0x8000, 0x8000,
						0x8000, 0x8000 };
VECT_VAR_DECL(expected_lt_64_1,int,32,4) [] = { 0x80000000, 0x80000000,
						0x80000000, 0x80000000 };
int VECT_VAR(expected_csat_lt_64_1,int,8,8) = 1;
int VECT_VAR(expected_csat_lt_64_1,int,16,4) = 1;
int VECT_VAR(expected_csat_lt_64_1,int,32,2) = 1;
int VECT_VAR(expected_csat_lt_64_1,int,8,16) = 1;
int VECT_VAR(expected_csat_lt_64_1,int,16,8) = 1;
int VECT_VAR(expected_csat_lt_64_1,int,32,4) = 1;

/* smaller types, corner cases causing cumulative saturation. (2)  */
VECT_VAR_DECL(expected_lt_64_2,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_lt_64_2,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_lt_64_2,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_lt_64_2,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_lt_64_2,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_lt_64_2,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
int VECT_VAR(expected_csat_lt_64_2,uint,8,8) = 1;
int VECT_VAR(expected_csat_lt_64_2,uint,16,4) = 1;
int VECT_VAR(expected_csat_lt_64_2,uint,32,2) = 1;
int VECT_VAR(expected_csat_lt_64_2,uint,8,16) = 1;
int VECT_VAR(expected_csat_lt_64_2,uint,16,8) = 1;
int VECT_VAR(expected_csat_lt_64_2,uint,32,4) = 1;

void vqsub_extras(void)
{
  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  /* Initialize input "vector1" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);

  /* Use a second vector full of 0.  */
  VDUP(vector2, , int, s, 64, 1, 0x0);
  VDUP(vector2, , uint, u, 64, 1, 0x0);
  VDUP(vector2, q, int, s, 64, 2, 0x0);
  VDUP(vector2, q, uint, u, 64, 2, 0x0);

#define MSG "64 bits saturation when adding zero"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, expected_cumulative_sat_64, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, expected_cumulative_sat_64, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, expected_cumulative_sat_64, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, expected_cumulative_sat_64, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64, MSG);

  /* Another set of tests with non-zero values.  */
  VDUP(vector2, , int, s, 64, 1, 0x44);
  VDUP(vector2, , uint, u, 64, 1, 0x88);
  VDUP(vector2, q, int, s, 64, 2, 0x44);
  VDUP(vector2, q, uint, u, 64, 2, 0x88);

#undef MSG
#define MSG "64 bits saturation cumulative_sat (2)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, expected_cumulative_sat_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, expected_cumulative_sat_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, expected_cumulative_sat_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, expected_cumulative_sat_64_2, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64_2, MSG);

  /* Another set of tests, with input values chosen to set
     cumulative_sat in all cases.  */
  VDUP(vector2, , int, s, 64, 1, 0x7fffffffffffffffLL);
  VDUP(vector2, , uint, u, 64, 1, 0xffffffffffffffffULL);
  /* To check positive saturation, we need to write a positive value
     in vector1.  */
  VDUP(vector1, q, int, s, 64, 2, 0x3fffffffffffffffLL);
  VDUP(vector2, q, int, s, 64, 2, 0x8000000000000000LL);
  VDUP(vector2, q, uint, u, 64, 2, 0xffffffffffffffffULL);

#undef MSG
#define MSG "64 bits saturation cumulative_sat (3)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, expected_cumulative_sat_64_3, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, expected_cumulative_sat_64_3, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, expected_cumulative_sat_64_3, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, expected_cumulative_sat_64_3, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64_3, MSG);

  /* To improve coverage, check saturation with less than 64 bits
     too.  */
  VDUP(vector2, , int, s, 8, 8, 0x7F);
  VDUP(vector2, , int, s, 16, 4, 0x7FFF);
  VDUP(vector2, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector2, q, int, s, 8, 16, 0x7F);
  VDUP(vector2, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector2, q, int, s, 32, 4, 0x7FFFFFFF);

#undef MSG
#define MSG "less than 64 bits saturation cumulative_sat (1)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 8, 8, expected_csat_lt_64_1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 16, 4, expected_csat_lt_64_1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 32, 2, expected_csat_lt_64_1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 8, 16, expected_csat_lt_64_1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 16, 8, expected_csat_lt_64_1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 32, 4, expected_csat_lt_64_1, MSG);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_lt_64_1, MSG);

  /* Another set of tests with vector1 values smaller than
     vector2.  */
  VDUP(vector1, , uint, u, 8, 8, 0x10);
  VDUP(vector1, , uint, u, 16, 4, 0x10);
  VDUP(vector1, , uint, u, 32, 2, 0x10);
  VDUP(vector1, q, uint, u, 8, 16, 0x10);
  VDUP(vector1, q, uint, u, 16, 8, 0x10);
  VDUP(vector1, q, uint, u, 32, 4, 0x10);

  VDUP(vector2, , uint, u, 8, 8, 0x20);
  VDUP(vector2, , uint, u, 16, 4, 0x20);
  VDUP(vector2, , uint, u, 32, 2, 0x20);
  VDUP(vector2, q, uint, u, 8, 16, 0x20);
  VDUP(vector2, q, uint, u, 16, 8, 0x20);
  VDUP(vector2, q, uint, u, 32, 4, 0x20);

#undef MSG
#define MSG "less than 64 bits saturation cumulative_sat (2)"
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 8, 8, expected_csat_lt_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 16, 4, expected_csat_lt_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 32, 2, expected_csat_lt_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 8, 16, expected_csat_lt_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 16, 8, expected_csat_lt_64_2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 32, 4, expected_csat_lt_64_2, MSG);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_lt_64_2, MSG);
}

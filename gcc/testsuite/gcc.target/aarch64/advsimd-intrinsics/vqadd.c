#define INSN_NAME vqadd
#define TEST_MSG "VQADD/VQADDQ"

/* Extra tests for special cases:
   - some requiring intermediate types larger than 64 bits to
   compute saturation flag.
   - corner case saturations with types smaller than 64 bits.
*/
void vqadd_extras(void);
#define EXTRA_TESTS vqadd_extras

#include "binary_sat_op.inc"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x1, 0x2, 0x3, 0x4,
				       0x5, 0x6, 0x7, 0x8 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x12, 0x13, 0x14, 0x15 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x23, 0x24 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x34 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x1, 0x2, 0x3, 0x4,
					0x5, 0x6, 0x7, 0x8,
					0x9, 0xa, 0xb, 0xc,
					0xd, 0xe, 0xf, 0x10 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x12, 0x13, 0x14, 0x15,
					0x16, 0x17, 0x18, 0x19 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x23, 0x24, 0x25, 0x26 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x34, 0x35 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffff, 0xffffffff,
					 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffffff,
					 0xffffffffffffffff };


/* 64-bits types, with 0 as second input.  */
VECT_VAR_DECL(expected_64,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_64,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_64,int,64,2) [] = { 0xfffffffffffffff0,
					   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_64,uint,64,2) [] = { 0xfffffffffffffff0,
					    0xfffffffffffffff1 };

/* 64-bits types, some cases causing cumulative saturation.  */
VECT_VAR_DECL(expected_64_2,int,64,1) [] = { 0x34 };
VECT_VAR_DECL(expected_64_2,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_64_2,int,64,2) [] = { 0x34, 0x35 };
VECT_VAR_DECL(expected_64_2,uint,64,2) [] = { 0xffffffffffffffff,
					      0xffffffffffffffff };

/* 64-bits types, all causing cumulative saturation.  */
VECT_VAR_DECL(expected_64_3,int,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_64_3,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_64_3,int,64,2) [] = { 0x7fffffffffffffff,
					     0x7fffffffffffffff };
VECT_VAR_DECL(expected_64_3,uint,64,2) [] = { 0xffffffffffffffff,
					      0xffffffffffffffff };

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

VECT_VAR_DECL(expected_lt_64_2,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_lt_64_2,uint,16,4) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_lt_64_2,uint,32,2) [] = { 0xffffffff,
						 0xffffffff };
VECT_VAR_DECL(expected_lt_64_2,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_lt_64_2,uint,16,8) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff,
						 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_lt_64_2,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						 0xffffffff, 0xffffffff };

void vqadd_extras(void)
{
  DECL_VARIABLE_ALL_VARIANTS(vector1);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  /* Initialize input "vector1" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector1, buffer);

  /* Use a second vector full of 0.  */
  VDUP(vector2, , int, s, 64, 1, 0);
  VDUP(vector2, , uint, u, 64, 1, 0);
  VDUP(vector2, q, int, s, 64, 2, 0);
  VDUP(vector2, q, uint, u, 64, 2, 0);

#define MSG "64 bits saturation adding zero"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1,MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64, MSG);

  /* Another set of tests with non-zero values, some chosen to create
     overflow.  */
  VDUP(vector2, , int, s, 64, 1, 0x44);
  VDUP(vector2, , uint, u, 64, 1, 0x88);
  VDUP(vector2, q, int, s, 64, 2, 0x44);
  VDUP(vector2, q, uint, u, 64, 2, 0x88);

#undef MSG
#define MSG "64 bits saturation cumulative_sat (2)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64_2, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64_2, MSG);

  /* Another set of tests, with input values chosen to set
     cumulative_sat in all cases.  */
  VDUP(vector2, , int, s, 64, 1, 0x8000000000000003LL);
  VDUP(vector2, , uint, u, 64, 1, 0x88);
  /* To check positive saturation, we need to write a positive value
     in vector1.  */
  VDUP(vector1, q, int, s, 64, 2, 0x4000000000000000LL);
  VDUP(vector2, q, int, s, 64, 2, 0x4000000000000000LL);
  VDUP(vector2, q, uint, u, 64, 2, 0x22);

#undef MSG
#define MSG "64 bits saturation cumulative_sat (3)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 64, 1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 64, 1, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 64, 2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 64, 2, MSG);

  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_64_3, MSG);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_64_3, MSG);

  /* To improve coverage, check saturation with less than 64 bits
     too.  */
  VDUP(vector2, , int, s, 8, 8, 0x81);
  VDUP(vector2, , int, s, 16, 4, 0x8001);
  VDUP(vector2, , int, s, 32, 2, 0x80000001);
  VDUP(vector2, q, int, s, 8, 16, 0x81);
  VDUP(vector2, q, int, s, 16, 8, 0x8001);
  VDUP(vector2, q, int, s, 32, 4, 0x80000001);

#undef MSG
#define MSG "less than 64 bits saturation cumulative_sat (1)"
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 8, 8, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 16, 4, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , int, s, 32, 2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 8, 16, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 16, 8, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, int, s, 32, 4, MSG);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_lt_64_1, MSG);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_lt_64_1, MSG);

  /* Another set of tests with large vector1 values.  */
  VDUP(vector1, , uint, u, 8, 8, 0xF0);
  VDUP(vector1, , uint, u, 16, 4, 0xFFF0);
  VDUP(vector1, , uint, u, 32, 2, 0xFFFFFFF0);
  VDUP(vector1, q, uint, u, 8, 16, 0xF0);
  VDUP(vector1, q, uint, u, 16, 8, 0xFFF0);
  VDUP(vector1, q, uint, u, 32, 4, 0xFFFFFFF0);

  VDUP(vector2, , uint, u, 8, 8, 0x20);
  VDUP(vector2, , uint, u, 16, 4, 0x20);
  VDUP(vector2, , uint, u, 32, 2, 0x20);
  VDUP(vector2, q, uint, u, 8, 16, 0x20);
  VDUP(vector2, q, uint, u, 16, 8, 0x20);
  VDUP(vector2, q, uint, u, 32, 4, 0x20);

#undef MSG
#define MSG "less than 64 bits saturation cumulative_sat (2)"
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 8, 8, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 16, 4, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, , uint, u, 32, 2, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 8, 16, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 16, 8, MSG);
  TEST_BINARY_SAT_OP(INSN_NAME, q, uint, u, 32, 4, MSG);

  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_lt_64_2, MSG);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_lt_64_2, MSG);
}

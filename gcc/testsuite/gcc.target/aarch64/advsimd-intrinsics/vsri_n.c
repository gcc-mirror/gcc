#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vsri
#define TEST_MSG "VSRI_N"

/* Extra tests for functions requiring corner cases tests.  */
void vsri_extra(void);
#define EXTRA_TESTS vsri_extra

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
				       0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x80000001, 0x80000001 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffff00000000 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xc5, 0xc5, 0xc5, 0xc5,
					0xc5, 0xc5, 0xc5, 0xc5 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffc0, 0xffc0, 0xffc0, 0xffc0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xe000000000000000 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xc5, 0xc5, 0xc5, 0xc5,
					0xc5, 0xc5, 0xc5, 0xc5 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xffc0, 0xffc0, 0xffc0, 0xffc0 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf7, 0xf7, 0xf7, 0xf7,
					0xf7, 0xf7, 0xf7, 0xf7,
					0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfffd, 0xfffd, 0xfffd, 0xfffd,
					0xfffd, 0xfffd, 0xfffd, 0xfffd };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffff, 0xffffffff,
					0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffff000000000000,
					0xffff000000000000 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0,
					 0xfff0, 0xfff0, 0xfff0, 0xfff0 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffe00, 0xfffffe00,
					 0xfffffe00, 0xfffffe00 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffff800,
					 0xfffffffffffff800 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1,
					 0xe1, 0xe1, 0xe1, 0xe1 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0xfff0, 0xfff0, 0xfff0,
					 0xfff0, 0xfff0, 0xfff0, 0xfff0 };

/* Expected results with max shift amount.  */
VECT_VAR_DECL(expected_max_shift,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						 0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_max_shift,int,16,4) [] = { 0xfff0, 0xfff1,
						  0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_max_shift,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_max_shift,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_max_shift,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_max_shift,uint,16,4) [] = { 0xfff0, 0xfff1,
						   0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_max_shift,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_max_shift,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_max_shift,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected_max_shift,poly,16,4) [] = { 0xfff0, 0xfff1,
						   0xfff2, 0xfff3 };
VECT_VAR_DECL(expected_max_shift,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						  0xf4, 0xf5, 0xf6, 0xf7,
						  0xf8, 0xf9, 0xfa, 0xfb,
						  0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_max_shift,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						  0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_max_shift,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
						  0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_max_shift,int,64,2) [] = { 0xfffffffffffffff0,
						  0xfffffffffffffff1 };
VECT_VAR_DECL(expected_max_shift,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						   0xf4, 0xf5, 0xf6, 0xf7,
						   0xf8, 0xf9, 0xfa, 0xfb,
						   0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_max_shift,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						   0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_max_shift,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						   0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_max_shift,uint,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_max_shift,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
						   0xf4, 0xf5, 0xf6, 0xf7,
						   0xf8, 0xf9, 0xfa, 0xfb,
						   0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected_max_shift,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						   0xfff4, 0xfff5, 0xfff6, 0xfff7 };

#include "vsXi_n.inc"

void vsri_extra(void)
{
  /* Test cases with maximum shift amount (this amount is different
     from vsli).  */

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Fill input vector2 with arbitrary values.  */
  VDUP(vector2, , int, s, 8, 8, 2);
  VDUP(vector2, , int, s, 16, 4, -4);
  VDUP(vector2, , int, s, 32, 2, 3);
  VDUP(vector2, , int, s, 64, 1, 100);
  VDUP(vector2, , uint, u, 8, 8, 20);
  VDUP(vector2, , uint, u, 16, 4, 30);
  VDUP(vector2, , uint, u, 32, 2, 40);
  VDUP(vector2, , uint, u, 64, 1, 2);
  VDUP(vector2, , poly, p, 8, 8, 20);
  VDUP(vector2, , poly, p, 16, 4, 30);
  VDUP(vector2, q, int, s, 8, 16, -10);
  VDUP(vector2, q, int, s, 16, 8, -20);
  VDUP(vector2, q, int, s, 32, 4, -30);
  VDUP(vector2, q, int, s, 64, 2, 24);
  VDUP(vector2, q, uint, u, 8, 16, 12);
  VDUP(vector2, q, uint, u, 16, 8, 3);
  VDUP(vector2, q, uint, u, 32, 4, 55);
  VDUP(vector2, q, uint, u, 64, 2, 3);
  VDUP(vector2, q, poly, p, 8, 16, 12);
  VDUP(vector2, q, poly, p, 16, 8, 3);

  /* Use maximum allowed shift amount.  */
  TEST_VSXI_N(INSN_NAME, , int, s, 8, 8, 8);
  TEST_VSXI_N(INSN_NAME, , int, s, 16, 4, 16);
  TEST_VSXI_N(INSN_NAME, , int, s, 32, 2, 32);
  TEST_VSXI_N(INSN_NAME, , int, s, 64, 1, 64);
  TEST_VSXI_N(INSN_NAME, , uint, u, 8, 8, 8);
  TEST_VSXI_N(INSN_NAME, , uint, u, 16, 4, 16);
  TEST_VSXI_N(INSN_NAME, , uint, u, 32, 2, 32);
  TEST_VSXI_N(INSN_NAME, , uint, u, 64, 1, 64);
  TEST_VSXI_N(INSN_NAME, , poly, p, 8, 8, 8);
  TEST_VSXI_N(INSN_NAME, , poly, p, 16, 4, 16);
  TEST_VSXI_N(INSN_NAME, q, int, s, 8, 16, 8);
  TEST_VSXI_N(INSN_NAME, q, int, s, 16, 8, 16);
  TEST_VSXI_N(INSN_NAME, q, int, s, 32, 4, 32);
  TEST_VSXI_N(INSN_NAME, q, int, s, 64, 2, 64);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 8, 16, 8);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 16, 8, 16);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 32, 4, 32);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 64, 2, 64);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 8, 16, 8);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 16, 8, 16);

#define COMMENT "(max shift amount)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected_max_shift, COMMENT);
}

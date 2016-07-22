#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

#define INSN_NAME vsli
#define TEST_MSG "VSLI_N"

/* Extra tests for functions requiring corner cases tests.  */
void vsli_extra(void);
#define EXTRA_TESTS vsli_extra

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0x20, 0x21, 0x22, 0x23,
				       0x24, 0x25, 0x26, 0x27 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffe0, 0xffe1, 0xffe2, 0xffe3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0x6, 0x7 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x64fffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x50, 0x51, 0x52, 0x53,
					0x50, 0x51, 0x52, 0x53 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x7bf0, 0x7bf1, 0x7bf2, 0x7bf3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x3ffffff0, 0x3ffffff1 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x10 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0x50, 0x51, 0x52, 0x53,
					0x50, 0x51, 0x52, 0x53 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0x7bf0, 0x7bf1, 0x7bf2, 0x7bf3 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xd0, 0xd1, 0xd2, 0xd3,
					0xd4, 0xd5, 0xd6, 0xd7,
					0xd8, 0xd9, 0xda, 0xdb,
					0xdc, 0xdd, 0xde, 0xdf };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xff60, 0xff61, 0xff62, 0xff63,
					0xff64, 0xff65, 0xff66, 0xff67 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfe2ffff0, 0xfe2ffff1,
					0xfe2ffff2, 0xfe2ffff3 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x18fff0, 0x18fff1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x60, 0x61, 0x62, 0x63,
					 0x64, 0x65, 0x66, 0x67,
					 0x60, 0x61, 0x62, 0x63,
					 0x64, 0x65, 0x66, 0x67 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3ff0, 0x3ff1, 0x3ff2, 0x3ff3,
					 0x3ff4, 0x3ff5, 0x3ff6, 0x3ff7 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x1bfffff0, 0x1bfffff1,
					 0x1bfffff2, 0x1bfffff3 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x7ffffffffffff0, 0x7ffffffffffff1 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x60, 0x61, 0x62, 0x63,
					 0x64, 0x65, 0x66, 0x67,
					 0x60, 0x61, 0x62, 0x63,
					 0x64, 0x65, 0x66, 0x67 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3ff0, 0x3ff1, 0x3ff2, 0x3ff3,
					 0x3ff4, 0x3ff5, 0x3ff6, 0x3ff7 };

/* Expected results with max shift amount.  */
VECT_VAR_DECL(expected_max_shift,int,8,8) [] = { 0x70, 0x71, 0x72, 0x73,
						 0x74, 0x75, 0x76, 0x77 };
VECT_VAR_DECL(expected_max_shift,int,16,4) [] = { 0x7ff0, 0x7ff1,
						  0x7ff2, 0x7ff3 };
VECT_VAR_DECL(expected_max_shift,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected_max_shift,int,64,1) [] = { 0x7ffffffffffffff0 };
VECT_VAR_DECL(expected_max_shift,uint,8,8) [] = { 0x70, 0x71, 0x72, 0x73,
						  0x74, 0x75, 0x76, 0x77 };
VECT_VAR_DECL(expected_max_shift,uint,16,4) [] = { 0x7ff0, 0x7ff1,
						   0x7ff2, 0x7ff3 };
VECT_VAR_DECL(expected_max_shift,uint,32,2) [] = { 0x7ffffff0, 0x7ffffff1 };
VECT_VAR_DECL(expected_max_shift,uint,64,1) [] = { 0x7ffffffffffffff0 };
VECT_VAR_DECL(expected_max_shift,poly,8,8) [] = { 0x70, 0x71, 0x72, 0x73,
						  0x74, 0x75, 0x76, 0x77 };
VECT_VAR_DECL(expected_max_shift,poly,16,4) [] = { 0x7ff0, 0x7ff1,
						   0x7ff2, 0x7ff3 };
VECT_VAR_DECL(expected_max_shift,int,8,16) [] = { 0x70, 0x71, 0x72, 0x73,
						  0x74, 0x75, 0x76, 0x77,
						  0x78, 0x79, 0x7a, 0x7b,
						  0x7c, 0x7d, 0x7e, 0x7f };
VECT_VAR_DECL(expected_max_shift,int,16,8) [] = { 0x7ff0, 0x7ff1, 0x7ff2, 0x7ff3,
						  0x7ff4, 0x7ff5, 0x7ff6, 0x7ff7 };
VECT_VAR_DECL(expected_max_shift,int,32,4) [] = { 0x7ffffff0, 0x7ffffff1,
						  0x7ffffff2, 0x7ffffff3 };
VECT_VAR_DECL(expected_max_shift,int,64,2) [] = { 0x7ffffffffffffff0,
						  0x7ffffffffffffff1 };
VECT_VAR_DECL(expected_max_shift,uint,8,16) [] = { 0x70, 0x71, 0x72, 0x73,
						   0x74, 0x75, 0x76, 0x77,
						   0x78, 0x79, 0x7a, 0x7b,
						   0x7c, 0x7d, 0x7e, 0x7f };
VECT_VAR_DECL(expected_max_shift,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						   0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected_max_shift,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
						   0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected_max_shift,uint,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_max_shift,poly,8,16) [] = { 0x70, 0x71, 0x72, 0x73,
						   0x74, 0x75, 0x76, 0x77,
						   0x78, 0x79, 0x7a, 0x7b,
						   0x7c, 0x7d, 0x7e, 0x7f };
VECT_VAR_DECL(expected_max_shift,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
						   0xfff4, 0xfff5, 0xfff6, 0xfff7 };

#include "vsXi_n.inc"

void vsli_extra(void)
{
  /* Test cases with maximum shift amount (this amount is different
     from vsri).  */

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
  TEST_VSXI_N(INSN_NAME, , int, s, 8, 8, 7);
  TEST_VSXI_N(INSN_NAME, , int, s, 16, 4, 15);
  TEST_VSXI_N(INSN_NAME, , int, s, 32, 2, 31);
  TEST_VSXI_N(INSN_NAME, , int, s, 64, 1, 63);
  TEST_VSXI_N(INSN_NAME, , uint, u, 8, 8, 7);
  TEST_VSXI_N(INSN_NAME, , uint, u, 16, 4, 15);
  TEST_VSXI_N(INSN_NAME, , uint, u, 32, 2, 31);
  TEST_VSXI_N(INSN_NAME, , uint, u, 64, 1, 63);
  TEST_VSXI_N(INSN_NAME, , poly, p, 8, 8, 7);
  TEST_VSXI_N(INSN_NAME, , poly, p, 16, 4, 15);
  TEST_VSXI_N(INSN_NAME, q, int, s, 8, 16, 7);
  TEST_VSXI_N(INSN_NAME, q, int, s, 16, 8, 15);
  TEST_VSXI_N(INSN_NAME, q, int, s, 32, 4, 31);
  TEST_VSXI_N(INSN_NAME, q, int, s, 64, 2, 63);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 8, 16, 7);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 16, 8, 15);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 32, 4, 31);
  TEST_VSXI_N(INSN_NAME, q, uint, u, 64, 2, 63);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 8, 16, 7);
  TEST_VSXI_N(INSN_NAME, q, poly, p, 16, 8, 15);

#define COMMENT "(max shift amount)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, poly, 8, 8, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, poly, 16, 4, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, poly, 8, 16, PRIx8, expected_max_shift, COMMENT);
  CHECK(TEST_MSG, poly, 16, 8, PRIx16, expected_max_shift, COMMENT);
}

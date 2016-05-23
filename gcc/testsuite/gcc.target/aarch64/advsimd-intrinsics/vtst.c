#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with signed input.  */
VECT_VAR_DECL(expected_signed,uint,8,8) [] = { 0x0, 0xff, 0xff, 0xff,
					       0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_signed,uint,16,4) [] = { 0x0, 0xffff, 0x0, 0xffff };
VECT_VAR_DECL(expected_signed,uint,32,2) [] = { 0x0, 0xffffffff };
VECT_VAR_DECL(expected_signed,uint,8,16) [] = { 0x0, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_signed,uint,16,8) [] = { 0x0, 0xffff, 0x0, 0xffff,
						0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_signed,uint,32,4) [] = { 0x0, 0xffffffff,
						0x0, 0xffffffff };

/* Expected results with unsigned input.  */
VECT_VAR_DECL(expected_unsigned,uint,8,8) [] = { 0x0, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_unsigned,uint,16,4) [] = { 0x0, 0xffff, 0x0, 0xffff };
VECT_VAR_DECL(expected_unsigned,uint,32,2) [] = { 0x0, 0xffffffff };
VECT_VAR_DECL(expected_unsigned,uint,8,16) [] = { 0x0, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_unsigned,uint,16,8) [] = { 0x0, 0xffff,
						  0x0, 0xffff,
						  0xffff, 0xffff,
						  0xffff, 0xffff };
VECT_VAR_DECL(expected_unsigned,uint,32,4) [] = { 0x0, 0xffffffff,
						  0x0, 0xffffffff };

/* Expected results with poly input.  */
VECT_VAR_DECL(expected_poly,uint,8,8) [] = { 0x0, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_poly,uint,8,16) [] = { 0x0, 0xff, 0xff, 0xff,
					      0xff, 0xff, 0xff, 0xff,
					      0xff, 0xff, 0xff, 0xff,
					      0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_poly,uint,16,4) [] = { 0x0, 0xffff, 0x0, 0xffff };
VECT_VAR_DECL(expected_poly,uint,16,8) [] = { 0x0, 0xffff,
					      0x0, 0xffff,
					      0xffff, 0xffff,
					      0xffff, 0xffff };

#define INSN_NAME vtst
#define TEST_MSG "VTST/VTSTQ"

/* We can't use the standard ref_v_binary_op.c template because vtst
   has no 64 bits variant, and outputs are always of uint type.  */
#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN_NAME)
{
  /* Basic test: y=OP(x,x), then store the result.  */
#define TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)		\
  VECT_VAR(vector_res, uint, W, N) =			\
    INSN##Q##_##T2##W(VECT_VAR(vector, T1, W, N),	\
		      VECT_VAR(vector2, T1, W, N));	\
  vst1##Q##_u##W(VECT_VAR(result, uint, W, N),		\
		 VECT_VAR(vector_res, uint, W, N))

#define TEST_BINARY_OP(INSN, Q, T1, T2, W, N)	\
  TEST_BINARY_OP1(INSN, Q, T1, T2, W, N)	\

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_UNSIGNED_VARIANTS(vector_res);


  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose init value arbitrarily, will be used as comparison
     value.  */
  VDUP(vector2, , int, s, 8, 8, 15);
  VDUP(vector2, , int, s, 16, 4, 5);
  VDUP(vector2, , int, s, 32, 2, 1);
  VDUP(vector2, , uint, u, 8, 8, 15);
  VDUP(vector2, , uint, u, 16, 4, 5);
  VDUP(vector2, , uint, u, 32, 2, 1);
  VDUP(vector2, , poly, p, 8, 8, 15);
  VDUP(vector2, , poly, p, 16, 4, 5);
  VDUP(vector2, q, int, s, 8, 16, 15);
  VDUP(vector2, q, int, s, 16, 8, 5);
  VDUP(vector2, q, int, s, 32, 4, 1);
  VDUP(vector2, q, uint, u, 8, 16, 15);
  VDUP(vector2, q, uint, u, 16, 8, 5);
  VDUP(vector2, q, uint, u, 32, 4, 1);
  VDUP(vector2, q, poly, p, 8, 16, 15);
  VDUP(vector2, q, poly, p, 16, 8, 5);

#define TEST_MACRO_NO64BIT_VARIANT_1_5(MACRO, VAR, T1, T2)	\
  MACRO(VAR, , T1, T2, 8, 8);					\
  MACRO(VAR, , T1, T2, 16, 4);					\
  MACRO(VAR, , T1, T2, 32, 2);					\
  MACRO(VAR, q, T1, T2, 8, 16);					\
  MACRO(VAR, q, T1, T2, 16, 8);					\
  MACRO(VAR, q, T1, T2, 32, 4)

  /* Split the test, as both signed and unsigned variants output their
     result in an unsigned form (thus the same output variable is used
     in these tests).  */
  TEST_MACRO_NO64BIT_VARIANT_1_5(TEST_BINARY_OP, INSN_NAME, int, s);

#define CMT " (signed input)"
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_signed, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_signed, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_signed, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_signed, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_signed, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_signed, CMT);

  TEST_MACRO_NO64BIT_VARIANT_1_5(TEST_BINARY_OP, INSN_NAME, uint, u);

#undef CMT
#define CMT " (unsigned input)"
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_unsigned, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_unsigned, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_unsigned, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_unsigned, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_unsigned, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_unsigned, CMT);

  /* Now, test the variants with poly8 and poly16 as input.  */
#undef CMT
#define CMT " (poly input)"
  TEST_BINARY_OP(INSN_NAME, , poly, p, 8, 8);
  TEST_BINARY_OP(INSN_NAME, , poly, p, 16, 4);
  TEST_BINARY_OP(INSN_NAME, q, poly, p, 8, 16);
  TEST_BINARY_OP(INSN_NAME, q, poly, p, 16, 8);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_poly, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_poly, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_poly, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_poly, CMT);
}

int main (void)
{
  exec_vtst ();
  return 0;
}

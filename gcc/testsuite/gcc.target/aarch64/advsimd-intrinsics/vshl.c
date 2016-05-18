#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xe0, 0xe2, 0xe4, 0xe6,
				       0xe8, 0xea, 0xec, 0xee };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xff80, 0xff88, 0xff90, 0xff98 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffff000, 0xfffff100 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffff80 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xe0, 0xe2, 0xe4, 0xe6,
					0xe8, 0xea, 0xec, 0xee };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xff80, 0xff88, 0xff90, 0xff98 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffff000, 0xfffff100 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffff80 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x0, 0x20, 0x40, 0x60,
					0x80, 0xa0, 0xc0, 0xe0,
					0x0, 0x20, 0x40, 0x60,
					0x80, 0xa0, 0xc0, 0xe0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x0, 0x1000, 0x2000, 0x3000,
					0x4000, 0x5000, 0x6000, 0x7000 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x0, 0x40000000,
					0x80000000, 0xc0000000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x0, 0x8000000000000000 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x0, 0x20, 0x40, 0x60,
					 0x80, 0xa0, 0xc0, 0xe0,
					 0x0, 0x20, 0x40, 0x60,
					 0x80, 0xa0, 0xc0, 0xe0 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x0, 0x1000, 0x2000, 0x3000,
					 0x4000, 0x5000, 0x6000, 0x7000 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x40000000,
					 0x80000000, 0xc0000000 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x0, 0x8000000000000000 };

/* Expected results with large shift amount.  */
VECT_VAR_DECL(expected_large_shift,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_large_shift,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_large_shift,uint,64,2) [] = { 0x0, 0x0 };


/* Expected results with negative shift amount.  */
VECT_VAR_DECL(expected_negative_shift,int,8,8) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
						      0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected_negative_shift,int,16,4) [] = { 0xfff8, 0xfff8,
						       0xfff9, 0xfff9  };
VECT_VAR_DECL(expected_negative_shift,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected_negative_shift,int,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_negative_shift,uint,8,8) [] = { 0x78, 0x78, 0x79, 0x79,
						       0x7a, 0x7a, 0x7b, 0x7b };
VECT_VAR_DECL(expected_negative_shift,uint,16,4) [] = { 0x7ff8, 0x7ff8,
							0x7ff9, 0x7ff9 };
VECT_VAR_DECL(expected_negative_shift,uint,32,2) [] = { 0x3ffffffc,
							0x3ffffffc };
VECT_VAR_DECL(expected_negative_shift,uint,64,1) [] = { 0xfffffffffffffff };
VECT_VAR_DECL(expected_negative_shift,int,8,16) [] = { 0xfc, 0xfc, 0xfc, 0xfc,
						       0xfd, 0xfd, 0xfd, 0xfd,
						       0xfe, 0xfe, 0xfe, 0xfe,
						       0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_negative_shift,int,16,8) [] = { 0xffff, 0xffff,
						       0xffff, 0xffff,
						       0xffff, 0xffff,
						       0xffff, 0xffff };
VECT_VAR_DECL(expected_negative_shift,int,32,4) [] = {  0xfffffffe, 0xfffffffe,
							0xfffffffe, 0xfffffffe };
VECT_VAR_DECL(expected_negative_shift,int,64,2) [] = { 0xffffffffffffffff,
						       0xffffffffffffffff };
VECT_VAR_DECL(expected_negative_shift,uint,8,16) [] = { 0x3c, 0x3c, 0x3c, 0x3c,
							0x3d, 0x3d, 0x3d, 0x3d,
							0x3e, 0x3e, 0x3e, 0x3e,
							0x3f, 0x3f, 0x3f, 0x3f };
VECT_VAR_DECL(expected_negative_shift,uint,16,8) [] = { 0x7ff, 0x7ff,
							0x7ff, 0x7ff,
							0x7ff, 0x7ff,
							0x7ff, 0x7ff };
VECT_VAR_DECL(expected_negative_shift,uint,32,4) [] = { 0x1ffffffe, 0x1ffffffe,
							0x1ffffffe, 0x1ffffffe };
VECT_VAR_DECL(expected_negative_shift,uint,64,2) [] = { 0x7ffffffffffffff,
							0x7ffffffffffffff };


#define INSN_NAME vshl
#define TEST_MSG "VSHL/VSHLQ"

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1(NAME)

void FNNAME (INSN_NAME) (void)
{
  /* Basic test: v3=vshl(v1,v2), then store the result.  */
#define TEST_VSHL(T3, Q, T1, T2, W, N)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vshl##Q##_##T2##W(VECT_VAR(vector, T1, W, N),			\
		      VECT_VAR(vector_shift, T3, W, N));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  DECL_VARIABLE_SIGNED_VARIANTS(vector_shift);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose init value arbitrarily, will be used as shift amount.  */
  VDUP(vector_shift, , int, s, 8, 8, 1);
  VDUP(vector_shift, , int, s, 16, 4, 3);
  VDUP(vector_shift, , int, s, 32, 2, 8);
  VDUP(vector_shift, , int, s, 64, 1, 3);
  VDUP(vector_shift, q, int, s, 8, 16, 5);
  VDUP(vector_shift, q, int, s, 16, 8, 12);
  VDUP(vector_shift, q, int, s, 32, 4, 30);
  VDUP(vector_shift, q, int, s, 64, 2, 63);

  /* Execute the tests.  */
  TEST_MACRO_ALL_VARIANTS_1_5(TEST_VSHL, int);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");


  /* Test large shift amount (larger or equal to the type width.  */
  VDUP(vector_shift, , int, s, 8, 8, 8);
  VDUP(vector_shift, , int, s, 16, 4, 16);
  VDUP(vector_shift, , int, s, 32, 2, 32);
  VDUP(vector_shift, , int, s, 64, 1, 64);
  VDUP(vector_shift, q, int, s, 8, 16, 8);
  VDUP(vector_shift, q, int, s, 16, 8, 17);
  VDUP(vector_shift, q, int, s, 32, 4, 33);
  VDUP(vector_shift, q, int, s, 64, 2, 65);

  /* Execute the tests.  */
  TEST_MACRO_ALL_VARIANTS_1_5(TEST_VSHL, int);

#define COMMENT1 "(large shift amount)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_large_shift, COMMENT1);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_large_shift, COMMENT1);


  /* Test negative shift amount. */
  VDUP(vector_shift, , int, s, 8, 8, -1);
  VDUP(vector_shift, , int, s, 16, 4, -1);
  VDUP(vector_shift, , int, s, 32, 2, -2);
  VDUP(vector_shift, , int, s, 64, 1, -4);
  VDUP(vector_shift, q, int, s, 8, 16, -2);
  VDUP(vector_shift, q, int, s, 16, 8, -5);
  VDUP(vector_shift, q, int, s, 32, 4, -3);
  VDUP(vector_shift, q, int, s, 64, 2, -5);

  /* Execute the tests.  */
  TEST_MACRO_ALL_VARIANTS_1_5(TEST_VSHL, int);

#define COMMENT2 "(negative shift amount)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_negative_shift, COMMENT2);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_negative_shift, COMMENT2);
}

int main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf9, 0xf9, 0xfa,
				       0xfa, 0xfb, 0xfb, 0xfc };
VECT_VAR_DECL(expected,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x3c, 0x3c, 0x3d, 0x3d,
					0x3d, 0x3d, 0x3e, 0x3e };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x1ffe, 0x1ffe, 0x1ffe, 0x1ffe };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x8000000, 0x8000000 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x80000000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf8, 0xf9, 0xf9, 0xfa,
					0xfa, 0xfb, 0xfb, 0xfc,
					0xfc, 0xfd, 0xfd, 0xfe,
					0xfe, 0xff, 0xff, 0x0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
					0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffc, 0xfffffffc,
					0xfffffffd, 0xfffffffd };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x3c, 0x3c, 0x3d, 0x3d,
					 0x3d, 0x3d, 0x3e, 0x3e,
					 0x3e, 0x3e, 0x3f, 0x3f,
					 0x3f, 0x3f, 0x40, 0x40 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1ffe, 0x1ffe, 0x1ffe, 0x1ffe,
					 0x1fff, 0x1fff, 0x1fff, 0x1fff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x8000000, 0x8000000,
					 0x8000000, 0x8000000 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x80000000, 0x80000000 };

/* Expected results with maximum input and max shift amount.  */
VECT_VAR_DECL(expected_max_sh_max,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_max_sh_max,uint,8,8) [] = { 0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,64,1) [] = { 0x1 };
VECT_VAR_DECL(expected_max_sh_max,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						   0x0, 0x0, 0x0, 0x0,
						   0x0, 0x0, 0x0, 0x0,
						   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						   0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_max_sh_max,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
						    0x1, 0x1, 0x1, 0x1,
						    0x1, 0x1, 0x1, 0x1,
						    0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,16,8) [] = { 0x1, 0x1, 0x1, 0x1,
						    0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,32,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_sh_max,uint,64,2) [] = { 0x1, 0x1 };

/* Expected results with maximum input and shift by 1.  */
VECT_VAR_DECL(expected_max_sh_1,int,8,8) [] = { 0x40, 0x40, 0x40, 0x40,
						0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_max_sh_1,int,16,4) [] = { 0x4000, 0x4000,
						 0x4000, 0x4000 };
VECT_VAR_DECL(expected_max_sh_1,int,32,2) [] = { 0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_max_sh_1,int,64,1) [] = { 0x4000000000000000 };
VECT_VAR_DECL(expected_max_sh_1,uint,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						 0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_sh_1,uint,16,4) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_sh_1,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_sh_1,uint,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_max_sh_1,int,8,16) [] = { 0x40, 0x40, 0x40, 0x40,
						 0x40, 0x40, 0x40, 0x40,
						 0x40, 0x40, 0x40, 0x40,
						 0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_max_sh_1,int,16,8) [] = { 0x4000, 0x4000,
						 0x4000, 0x4000,
						 0x4000, 0x4000,
						 0x4000, 0x4000 };
VECT_VAR_DECL(expected_max_sh_1,int,32,4) [] = { 0x40000000, 0x40000000,
						 0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_max_sh_1,int,64,2) [] = { 0x4000000000000000,
						 0x4000000000000000 };
VECT_VAR_DECL(expected_max_sh_1,uint,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80,
						  0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_sh_1,uint,16,8) [] = { 0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000,
						  0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_sh_1,uint,32,4) [] = { 0x80000000, 0x80000000,
						  0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_sh_1,uint,64,2) [] = { 0x8000000000000000,
						  0x8000000000000000 };

/* Expected results with maximum input and shift by 3.  */
VECT_VAR_DECL(expected_max_sh_3,int,8,8) [] = { 0x10, 0x10, 0x10, 0x10,
						0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_max_sh_3,int,16,4) [] = { 0x1000, 0x1000,
						 0x1000, 0x1000 };
VECT_VAR_DECL(expected_max_sh_3,int,32,2) [] = { 0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_max_sh_3,int,64,1) [] = { 0x1000000000000000 };
VECT_VAR_DECL(expected_max_sh_3,uint,8,8) [] = { 0x20, 0x20, 0x20, 0x20,
						 0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_sh_3,uint,16,4) [] = { 0x2000, 0x2000,
						  0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_sh_3,uint,32,2) [] = { 0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_sh_3,uint,64,1) [] = { 0x2000000000000000 };
VECT_VAR_DECL(expected_max_sh_3,int,8,16) [] = { 0x10, 0x10, 0x10, 0x10,
						 0x10, 0x10, 0x10, 0x10,
						 0x10, 0x10, 0x10, 0x10,
						 0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_max_sh_3,int,16,8) [] = { 0x1000, 0x1000,
						 0x1000, 0x1000,
						 0x1000, 0x1000,
						 0x1000, 0x1000 };
VECT_VAR_DECL(expected_max_sh_3,int,32,4) [] = { 0x10000000, 0x10000000,
						 0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_max_sh_3,int,64,2) [] = { 0x1000000000000000,
						 0x1000000000000000 };
VECT_VAR_DECL(expected_max_sh_3,uint,8,16) [] = { 0x20, 0x20, 0x20, 0x20,
						  0x20, 0x20, 0x20, 0x20,
						  0x20, 0x20, 0x20, 0x20,
						  0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_sh_3,uint,16,8) [] = { 0x2000, 0x2000,
						  0x2000, 0x2000,
						  0x2000, 0x2000,
						  0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_sh_3,uint,32,4) [] = { 0x20000000, 0x20000000,
						  0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_sh_3,uint,64,2) [] = { 0x2000000000000000,
						  0x2000000000000000 };

/* Expected results with max negative input (for signed types, shift
   by 1.  */
VECT_VAR_DECL(expected_max_neg_sh_1,int,8,8) [] = { 0xc0, 0xc0, 0xc0, 0xc0,
						    0xc0, 0xc0, 0xc0, 0xc0 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,16,4) [] = { 0xc000, 0xc000,
						     0xc000, 0xc000 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,32,2) [] = { 0xc0000000, 0xc0000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,64,1) [] = { 0xc000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						     0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,16,4) [] = { 0x8000, 0x8000,
						      0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,8,16) [] = { 0xc0, 0xc0, 0xc0, 0xc0,
						     0xc0, 0xc0, 0xc0, 0xc0,
						     0xc0, 0xc0, 0xc0, 0xc0,
						     0xc0, 0xc0, 0xc0, 0xc0 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,16,8) [] = { 0xc000, 0xc000,
						     0xc000, 0xc000,
						     0xc000, 0xc000,
						     0xc000, 0xc000 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,32,4) [] = { 0xc0000000, 0xc0000000,
						     0xc0000000, 0xc0000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,int,64,2) [] = { 0xc000000000000000,
						     0xc000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						      0x80, 0x80, 0x80, 0x80,
						      0x80, 0x80, 0x80, 0x80,
						      0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,16,8) [] = { 0x8000, 0x8000,
						      0x8000, 0x8000,
						      0x8000, 0x8000,
						      0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,32,4) [] = { 0x80000000, 0x80000000,
						      0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_neg_sh_1,uint,64,2) [] = { 0x8000000000000000,
						      0x8000000000000000 };

/* Expected results with max negative input (for signed types, shift
   by 3.  */
VECT_VAR_DECL(expected_max_neg_sh_3,int,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
						    0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,16,4) [] = { 0xf000, 0xf000,
						     0xf000, 0xf000 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,32,2) [] = { 0xf0000000, 0xf0000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,64,1) [] = { 0xf000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,8,8) [] = { 0x20, 0x20, 0x20, 0x20,
						     0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,16,4) [] = { 0x2000, 0x2000,
						      0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,32,2) [] = { 0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,64,1) [] = { 0x2000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
						     0xf0, 0xf0, 0xf0, 0xf0,
						     0xf0, 0xf0, 0xf0, 0xf0,
						     0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,16,8) [] = { 0xf000, 0xf000,
						     0xf000, 0xf000,
						     0xf000, 0xf000,
						     0xf000, 0xf000 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,32,4) [] = { 0xf0000000, 0xf0000000,
						     0xf0000000, 0xf0000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,int,64,2) [] = { 0xf000000000000000,
						     0xf000000000000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,8,16) [] = { 0x20, 0x20, 0x20, 0x20,
						      0x20, 0x20, 0x20, 0x20,
						      0x20, 0x20, 0x20, 0x20,
						      0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,16,8) [] = { 0x2000, 0x2000,
						      0x2000, 0x2000,
						      0x2000, 0x2000,
						      0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,32,4) [] = { 0x20000000, 0x20000000,
						      0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_neg_sh_3,uint,64,2) [] = { 0x2000000000000000,
						      0x2000000000000000 };

#define TEST_MSG "VRSHR_N"
void exec_vrshr_n (void)
{
  /* Basic test: y=vrshr_n(x,v), then store the result.  */
#define TEST_VRSHR_N(Q, T1, T2, W, N, V)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    vrshr##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			 V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSHR_N(, int, s, 8, 8, 1);
  TEST_VRSHR_N(, int, s, 16, 4, 12);
  TEST_VRSHR_N(, int, s, 32, 2, 2);
  TEST_VRSHR_N(, int, s, 64, 1, 32);
  TEST_VRSHR_N(, uint, u, 8, 8, 2);
  TEST_VRSHR_N(, uint, u, 16, 4, 3);
  TEST_VRSHR_N(, uint, u, 32, 2, 5);
  TEST_VRSHR_N(, uint, u, 64, 1, 33);

  TEST_VRSHR_N(q, int, s, 8, 16, 1);
  TEST_VRSHR_N(q, int, s, 16, 8, 12);
  TEST_VRSHR_N(q, int, s, 32, 4, 2);
  TEST_VRSHR_N(q, int, s, 64, 2, 32);
  TEST_VRSHR_N(q, uint, u, 8, 16, 2);
  TEST_VRSHR_N(q, uint, u, 16, 8, 3);
  TEST_VRSHR_N(q, uint, u, 32, 4, 5);
  TEST_VRSHR_N(q, uint, u, 64, 2, 33);

#define CMT ""
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, CMT);


  /* Use maximum positive input value.  */
  VDUP(vector, , int, s, 8, 8, 0x7F);
  VDUP(vector, , int, s, 16, 4, 0x7FFF);
  VDUP(vector, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, , uint, u, 8, 8, 0xFF);
  VDUP(vector, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, , uint, u, 64, 1, 0xFFFFFFFFFFFFFFFFULL);
  VDUP(vector, q, int, s, 8, 16, 0x7F);
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, uint, u, 8, 16, 0xFF);
  VDUP(vector, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

  /* Use max shift amount, to exercise saturation.  */
  TEST_VRSHR_N(, int, s, 8, 8, 8);
  TEST_VRSHR_N(, int, s, 16, 4, 16);
  TEST_VRSHR_N(, int, s, 32, 2, 32);
  TEST_VRSHR_N(, int, s, 64, 1, 64);
  TEST_VRSHR_N(, uint, u, 8, 8, 8);
  TEST_VRSHR_N(, uint, u, 16, 4, 16);
  TEST_VRSHR_N(, uint, u, 32, 2, 32);
  TEST_VRSHR_N(, uint, u, 64, 1, 64);
  TEST_VRSHR_N(q, int, s, 8, 16, 8);
  TEST_VRSHR_N(q, int, s, 16, 8, 16);
  TEST_VRSHR_N(q, int, s, 32, 4, 32);
  TEST_VRSHR_N(q, int, s, 64, 2, 64);
  TEST_VRSHR_N(q, uint, u, 8, 16, 8);
  TEST_VRSHR_N(q, uint, u, 16, 8, 16);
  TEST_VRSHR_N(q, uint, u, 32, 4, 32);
  TEST_VRSHR_N(q, uint, u, 64, 2, 64);

#undef CMT
#define CMT " (overflow test: max shift amount, max positive input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh_max, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_sh_max, CMT);


  /* Use 1 as shift amount, to exercise saturation.  */
  TEST_VRSHR_N(, int, s, 8, 8, 1);
  TEST_VRSHR_N(, int, s, 16, 4, 1);
  TEST_VRSHR_N(, int, s, 32, 2, 1);
  TEST_VRSHR_N(, int, s, 64, 1, 1);
  TEST_VRSHR_N(, uint, u, 8, 8, 1);
  TEST_VRSHR_N(, uint, u, 16, 4, 1);
  TEST_VRSHR_N(, uint, u, 32, 2, 1);
  TEST_VRSHR_N(, uint, u, 64, 1, 1);
  TEST_VRSHR_N(q, int, s, 8, 16, 1);
  TEST_VRSHR_N(q, int, s, 16, 8, 1);
  TEST_VRSHR_N(q, int, s, 32, 4, 1);
  TEST_VRSHR_N(q, int, s, 64, 2, 1);
  TEST_VRSHR_N(q, uint, u, 8, 16, 1);
  TEST_VRSHR_N(q, uint, u, 16, 8, 1);
  TEST_VRSHR_N(q, uint, u, 32, 4, 1);
  TEST_VRSHR_N(q, uint, u, 64, 2, 1);

#undef CMT
#define CMT " (overflow test: shift by 1, with max input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh_1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_sh_1, CMT);


  /* Use 3 as shift amount, to exercise saturation.  */
  TEST_VRSHR_N(, int, s, 8, 8, 3);
  TEST_VRSHR_N(, int, s, 16, 4, 3);
  TEST_VRSHR_N(, int, s, 32, 2, 3);
  TEST_VRSHR_N(, int, s, 64, 1, 3);
  TEST_VRSHR_N(, uint, u, 8, 8, 3);
  TEST_VRSHR_N(, uint, u, 16, 4, 3);
  TEST_VRSHR_N(, uint, u, 32, 2, 3);
  TEST_VRSHR_N(, uint, u, 64, 1, 3);
  TEST_VRSHR_N(q, int, s, 8, 16, 3);
  TEST_VRSHR_N(q, int, s, 16, 8, 3);
  TEST_VRSHR_N(q, int, s, 32, 4, 3);
  TEST_VRSHR_N(q, int, s, 64, 2, 3);
  TEST_VRSHR_N(q, uint, u, 8, 16, 3);
  TEST_VRSHR_N(q, uint, u, 16, 8, 3);
  TEST_VRSHR_N(q, uint, u, 32, 4, 3);
  TEST_VRSHR_N(q, uint, u, 64, 2, 3);

#undef CMT
#define CMT " (overflow test: shift by 3, with max input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh_3, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_sh_3, CMT);


  /* Use minimum negative input for signed types.  */
  VDUP(vector, , int, s, 8, 8, 0x80);
  VDUP(vector, , int, s, 16, 4, 0x8000);
  VDUP(vector, , int, s, 32, 2, 0x80000000);
  VDUP(vector, , int, s, 64, 1, 0x8000000000000000LL);
  VDUP(vector, , uint, u, 8, 8, 0xFF);
  VDUP(vector, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, , uint, u, 64, 1, 0xFFFFFFFFFFFFFFFFULL);
  VDUP(vector, q, int, s, 8, 16, 0x80);
  VDUP(vector, q, int, s, 16, 8, 0x8000);
  VDUP(vector, q, int, s, 32, 4, 0x80000000);
  VDUP(vector, q, int, s, 64, 2, 0x8000000000000000LL);
  VDUP(vector, q, uint, u, 8, 16, 0xFF);
  VDUP(vector, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);


  /* Use 1 as shift amount, to exercise saturation code.  */
  TEST_VRSHR_N(, int, s, 8, 8, 1);
  TEST_VRSHR_N(, int, s, 16, 4, 1);
  TEST_VRSHR_N(, int, s, 32, 2, 1);
  TEST_VRSHR_N(, int, s, 64, 1, 1);
  TEST_VRSHR_N(, uint, u, 8, 8, 1);
  TEST_VRSHR_N(, uint, u, 16, 4, 1);
  TEST_VRSHR_N(, uint, u, 32, 2, 1);
  TEST_VRSHR_N(, uint, u, 64, 1, 1);
  TEST_VRSHR_N(q, int, s, 8, 16, 1);
  TEST_VRSHR_N(q, int, s, 16, 8, 1);
  TEST_VRSHR_N(q, int, s, 32, 4, 1);
  TEST_VRSHR_N(q, int, s, 64, 2, 1);
  TEST_VRSHR_N(q, uint, u, 8, 16, 1);
  TEST_VRSHR_N(q, uint, u, 16, 8, 1);
  TEST_VRSHR_N(q, uint, u, 32, 4, 1);
  TEST_VRSHR_N(q, uint, u, 64, 2, 1);

#undef CMT
#define CMT " (overflow test: shift by 1, with negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_neg_sh_1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_neg_sh_1, CMT);


  /* Use 3 as shift amount, to exercise saturation code.  */
  TEST_VRSHR_N(, int, s, 8, 8, 3);
  TEST_VRSHR_N(, int, s, 16, 4, 3);
  TEST_VRSHR_N(, int, s, 32, 2, 3);
  TEST_VRSHR_N(, int, s, 64, 1, 3);
  TEST_VRSHR_N(, uint, u, 8, 8, 3);
  TEST_VRSHR_N(, uint, u, 16, 4, 3);
  TEST_VRSHR_N(, uint, u, 32, 2, 3);
  TEST_VRSHR_N(, uint, u, 64, 1, 3);
  TEST_VRSHR_N(q, int, s, 8, 16, 3);
  TEST_VRSHR_N(q, int, s, 16, 8, 3);
  TEST_VRSHR_N(q, int, s, 32, 4, 3);
  TEST_VRSHR_N(q, int, s, 64, 2, 3);
  TEST_VRSHR_N(q, uint, u, 8, 16, 3);
  TEST_VRSHR_N(q, uint, u, 16, 8, 3);
  TEST_VRSHR_N(q, uint, u, 32, 4, 3);
  TEST_VRSHR_N(q, uint, u, 64, 2, 3);

#undef CMT
#define CMT " (overflow test: shift by 3, with negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_neg_sh_3, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_neg_sh_3, CMT);
}

int main (void)
{
  exec_vrshr_n ();
  return 0;
}

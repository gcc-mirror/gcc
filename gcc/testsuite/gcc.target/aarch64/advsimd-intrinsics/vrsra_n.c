#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf9, 0xfa, 0xfb, 0xfc,
				       0xfd, 0xfe, 0xff, 0x0 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffd, 0xfffffffe };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x5, 0x6, 0x7, 0x8,
					0x9, 0xa, 0xb, 0xc };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfffd, 0xfffe, 0xffff, 0x0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff4, 0xfffffff5 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf9, 0xfa, 0xfb, 0xfc,
					0xfd, 0xfe, 0xff, 0x0,
					0x1, 0x2, 0x3, 0x4,
					0x5, 0x6, 0x7, 0x8 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffd, 0xfffffffe,
					0xffffffff, 0x0 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0, 0xfffffffffffffff1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x5, 0x6, 0x7, 0x8,
					 0x9, 0xa, 0xb, 0xc,
					 0xd, 0xe, 0xf, 0x10,
					 0x11, 0x12, 0x13, 0x14 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfffd, 0xfffe, 0xffff, 0x0,
					 0x1, 0x2, 0x3, 0x4 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff4, 0xfffffff5,
					 0xfffffff6, 0xfffffff7 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff1 };

/* Expected results with max input and shift by 1.  */
VECT_VAR_DECL(expected_max_sh1,int,8,8) [] = { 0x40, 0x40, 0x40, 0x40,
					       0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_max_sh1,int,16,4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected_max_sh1,int,32,2) [] = { 0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_max_sh1,int,64,1) [] = { 0x4000000000000000 };
VECT_VAR_DECL(expected_max_sh1,uint,8,8) [] = { 0x80, 0x80, 0x80, 0x80,
						0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_sh1,uint,16,4) [] = { 0x8000, 0x8000,
						 0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_sh1,uint,32,2) [] = { 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_sh1,uint,64,1) [] = { 0x8000000000000000 };
VECT_VAR_DECL(expected_max_sh1,int,8,16) [] = { 0x40, 0x40, 0x40, 0x40,
						0x40, 0x40, 0x40, 0x40,
						0x40, 0x40, 0x40, 0x40,
						0x40, 0x40, 0x40, 0x40 };
VECT_VAR_DECL(expected_max_sh1,int,16,8) [] = { 0x4000, 0x4000, 0x4000, 0x4000,
						0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected_max_sh1,int,32,4) [] = { 0x40000000, 0x40000000,
						0x40000000, 0x40000000 };
VECT_VAR_DECL(expected_max_sh1,int,64,2) [] = { 0x4000000000000000,
						0x4000000000000000 };
VECT_VAR_DECL(expected_max_sh1,uint,8,16) [] = { 0x80, 0x80, 0x80, 0x80,
						 0x80, 0x80, 0x80, 0x80,
						 0x80, 0x80, 0x80, 0x80,
						 0x80, 0x80, 0x80, 0x80 };
VECT_VAR_DECL(expected_max_sh1,uint,16,8) [] = { 0x8000, 0x8000,
						 0x8000, 0x8000,
						 0x8000, 0x8000,
						 0x8000, 0x8000 };
VECT_VAR_DECL(expected_max_sh1,uint,32,4) [] = { 0x80000000, 0x80000000,
						 0x80000000, 0x80000000 };
VECT_VAR_DECL(expected_max_sh1,uint,64,2) [] = { 0x8000000000000000,
						 0x8000000000000000 };

/* Expected results with max input and shift by 3.  */
VECT_VAR_DECL(expected_max_sh3,int,8,8) [] = { 0x10, 0x10, 0x10, 0x10,
					       0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_max_sh3,int,16,4) [] = { 0x1000, 0x1000, 0x1000, 0x1000 };
VECT_VAR_DECL(expected_max_sh3,int,32,2) [] = { 0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_max_sh3,int,64,1) [] = { 0x1000000000000000 };
VECT_VAR_DECL(expected_max_sh3,uint,8,8) [] = { 0x20, 0x20, 0x20, 0x20,
						0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_sh3,uint,16,4) [] = { 0x2000, 0x2000,
						 0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_sh3,uint,32,2) [] = { 0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_sh3,uint,64,1) [] = { 0x2000000000000000 };
VECT_VAR_DECL(expected_max_sh3,int,8,16) [] = { 0x10, 0x10, 0x10, 0x10,
						0x10, 0x10, 0x10, 0x10,
						0x10, 0x10, 0x10, 0x10,
						0x10, 0x10, 0x10, 0x10 };
VECT_VAR_DECL(expected_max_sh3,int,16,8) [] = { 0x1000, 0x1000, 0x1000, 0x1000,
						0x1000, 0x1000, 0x1000, 0x1000 };
VECT_VAR_DECL(expected_max_sh3,int,32,4) [] = { 0x10000000, 0x10000000,
						0x10000000, 0x10000000 };
VECT_VAR_DECL(expected_max_sh3,int,64,2) [] = { 0x1000000000000000,
						0x1000000000000000 };
VECT_VAR_DECL(expected_max_sh3,uint,8,16) [] = { 0x20, 0x20, 0x20, 0x20,
						 0x20, 0x20, 0x20, 0x20,
						 0x20, 0x20, 0x20, 0x20,
						 0x20, 0x20, 0x20, 0x20 };
VECT_VAR_DECL(expected_max_sh3,uint,16,8) [] = { 0x2000, 0x2000,
						 0x2000, 0x2000,
						 0x2000, 0x2000,
						 0x2000, 0x2000 };
VECT_VAR_DECL(expected_max_sh3,uint,32,4) [] = { 0x20000000, 0x20000000,
						 0x20000000, 0x20000000 };
VECT_VAR_DECL(expected_max_sh3,uint,64,2) [] = { 0x2000000000000000,
						 0x2000000000000000 };

/* Expected results with max input and shift by type width.  */
VECT_VAR_DECL(expected_max_shmax,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_max_shmax,uint,8,8) [] = { 0x1, 0x1, 0x1, 0x1,
						  0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,64,1) [] = { 0x1 };
VECT_VAR_DECL(expected_max_shmax,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_max_shmax,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,16,8) [] = { 0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,32,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_max_shmax,uint,64,2) [] = { 0x1, 0x1 };

/* Expected results with min negative input and shift by 1.  */
VECT_VAR_DECL(expected_min_sh1,int,8,8) [] = { 0xc0, 0xc0, 0xc0, 0xc0,
					       0xc0, 0xc0, 0xc0, 0xc0 };
VECT_VAR_DECL(expected_min_sh1,int,16,4) [] = { 0xc000, 0xc000, 0xc000, 0xc000 };
VECT_VAR_DECL(expected_min_sh1,int,32,2) [] = { 0xc0000000, 0xc0000000 };
VECT_VAR_DECL(expected_min_sh1,int,64,1) [] = { 0xc000000000000000 };
VECT_VAR_DECL(expected_min_sh1,uint,8,8) [] = { 0x1, 0x1, 0x1, 0x1,
						0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,64,1) [] = { 0x1 };
VECT_VAR_DECL(expected_min_sh1,int,8,16) [] = { 0xc0, 0xc0, 0xc0, 0xc0,
						0xc0, 0xc0, 0xc0, 0xc0,
						0xc0, 0xc0, 0xc0, 0xc0,
						0xc0, 0xc0, 0xc0, 0xc0 };
VECT_VAR_DECL(expected_min_sh1,int,16,8) [] = { 0xc000, 0xc000, 0xc000, 0xc000,
						0xc000, 0xc000, 0xc000, 0xc000 };
VECT_VAR_DECL(expected_min_sh1,int,32,4) [] = { 0xc0000000, 0xc0000000,
						0xc0000000, 0xc0000000 };
VECT_VAR_DECL(expected_min_sh1,int,64,2) [] = { 0xc000000000000000,
						0xc000000000000000 };
VECT_VAR_DECL(expected_min_sh1,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,16,8) [] = { 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,32,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh1,uint,64,2) [] = { 0x1, 0x1 };

/* Expected results with min negative input and shift by 3.  */
VECT_VAR_DECL(expected_min_sh3,int,8,8) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
					       0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected_min_sh3,int,16,4) [] = { 0xf000, 0xf000, 0xf000, 0xf000 };
VECT_VAR_DECL(expected_min_sh3,int,32,2) [] = { 0xf0000000, 0xf0000000 };
VECT_VAR_DECL(expected_min_sh3,int,64,1) [] = { 0xf000000000000000 };
VECT_VAR_DECL(expected_min_sh3,uint,8,8) [] = { 0x1, 0x1, 0x1, 0x1,
						0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,64,1) [] = { 0x1 };
VECT_VAR_DECL(expected_min_sh3,int,8,16) [] = { 0xf0, 0xf0, 0xf0, 0xf0,
						0xf0, 0xf0, 0xf0, 0xf0,
						0xf0, 0xf0, 0xf0, 0xf0,
						0xf0, 0xf0, 0xf0, 0xf0 };
VECT_VAR_DECL(expected_min_sh3,int,16,8) [] = { 0xf000, 0xf000, 0xf000, 0xf000,
						0xf000, 0xf000, 0xf000, 0xf000 };
VECT_VAR_DECL(expected_min_sh3,int,32,4) [] = { 0xf0000000, 0xf0000000,
						0xf0000000, 0xf0000000 };
VECT_VAR_DECL(expected_min_sh3,int,64,2) [] = { 0xf000000000000000,
						0xf000000000000000 };
VECT_VAR_DECL(expected_min_sh3,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,16,8) [] = { 0x1, 0x1, 0x1, 0x1,
						 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,32,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_sh3,uint,64,2) [] = { 0x1, 0x1 };

/* Expected results with min negative input and shift by type width.  */
VECT_VAR_DECL(expected_min_shmax,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,64,1) [] = { 0x0 };
VECT_VAR_DECL(expected_min_shmax,uint,8,8) [] = { 0x1, 0x1, 0x1, 0x1,
						  0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,16,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,32,2) [] = { 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,64,1) [] = { 0x1 };
VECT_VAR_DECL(expected_min_shmax,int,8,16) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,16,8) [] = { 0x0, 0x0, 0x0, 0x0,
						  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,32,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,int,64,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_min_shmax,uint,8,16) [] = { 0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,16,8) [] = { 0x1, 0x1, 0x1, 0x1,
						   0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,32,4) [] = { 0x1, 0x1, 0x1, 0x1 };
VECT_VAR_DECL(expected_min_shmax,uint,64,2) [] = { 0x1, 0x1 };

#define TEST_MSG "VRSRA_N"
void exec_vrsra_n (void)
{
  /* Basic test: y=vrsra_n(x,v), then store the result.  */
#define TEST_VRSRA_N(Q, T1, T2, W, N, V)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    vrsra##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			 VECT_VAR(vector2, T1, W, N),			\
			 V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector2);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose arbitrary initialization values.  */
  VDUP(vector2, , int, s, 8, 8, 0x11);
  VDUP(vector2, , int, s, 16, 4, 0x22);
  VDUP(vector2, , int, s, 32, 2, 0x33);
  VDUP(vector2, , int, s, 64, 1, 0x44);
  VDUP(vector2, , uint, u, 8, 8, 0x55);
  VDUP(vector2, , uint, u, 16, 4, 0x66);
  VDUP(vector2, , uint, u, 32, 2, 0x77);
  VDUP(vector2, , uint, u, 64, 1, 0x88);

  VDUP(vector2, q, int, s, 8, 16, 0x11);
  VDUP(vector2, q, int, s, 16, 8, 0x22);
  VDUP(vector2, q, int, s, 32, 4, 0x33);
  VDUP(vector2, q, int, s, 64, 2, 0x44);
  VDUP(vector2, q, uint, u, 8, 16, 0x55);
  VDUP(vector2, q, uint, u, 16, 8, 0x66);
  VDUP(vector2, q, uint, u, 32, 4, 0x77);
  VDUP(vector2, q, uint, u, 64, 2, 0x88);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSRA_N(, int, s, 8, 8, 1);
  TEST_VRSRA_N(, int, s, 16, 4, 12);
  TEST_VRSRA_N(, int, s, 32, 2, 2);
  TEST_VRSRA_N(, int, s, 64, 1, 32);
  TEST_VRSRA_N(, uint, u, 8, 8, 2);
  TEST_VRSRA_N(, uint, u, 16, 4, 3);
  TEST_VRSRA_N(, uint, u, 32, 2, 5);
  TEST_VRSRA_N(, uint, u, 64, 1, 33);

  TEST_VRSRA_N(q, int, s, 8, 16, 1);
  TEST_VRSRA_N(q, int, s, 16, 8, 12);
  TEST_VRSRA_N(q, int, s, 32, 4, 2);
  TEST_VRSRA_N(q, int, s, 64, 2, 32);
  TEST_VRSRA_N(q, uint, u, 8, 16, 2);
  TEST_VRSRA_N(q, uint, u, 16, 8, 3);
  TEST_VRSRA_N(q, uint, u, 32, 4, 5);
  TEST_VRSRA_N(q, uint, u, 64, 2, 33);

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


  /* Initialize the accumulator with 0.  */
  VDUP(vector, , int, s, 8, 8, 0);
  VDUP(vector, , int, s, 16, 4, 0);
  VDUP(vector, , int, s, 32, 2, 0);
  VDUP(vector, , int, s, 64, 1, 0);
  VDUP(vector, , uint, u, 8, 8, 0);
  VDUP(vector, , uint, u, 16, 4, 0);
  VDUP(vector, , uint, u, 32, 2, 0);
  VDUP(vector, , uint, u, 64, 1, 0);
  VDUP(vector, q, int, s, 8, 16, 0);
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 8, 16, 0);
  VDUP(vector, q, uint, u, 16, 8, 0);
  VDUP(vector, q, uint, u, 32, 4, 0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Initialize with max values to check overflow.  */
  VDUP(vector2, , int, s, 8, 8, 0x7F);
  VDUP(vector2, , int, s, 16, 4, 0x7FFF);
  VDUP(vector2, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector2, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector2, , uint, u, 8, 8, 0xFF);
  VDUP(vector2, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector2, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector2, , uint, u, 64, 1, 0xFFFFFFFFFFFFFFFFULL);
  VDUP(vector2, q, int, s, 8, 16, 0x7F);
  VDUP(vector2, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector2, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector2, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector2, q, uint, u, 8, 16, 0xFF);
  VDUP(vector2, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector2, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector2, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

  /* Shift by 1 to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 1);
  TEST_VRSRA_N(, int, s, 16, 4, 1);
  TEST_VRSRA_N(, int, s, 32, 2, 1);
  TEST_VRSRA_N(, int, s, 64, 1, 1);
  TEST_VRSRA_N(, uint, u, 8, 8, 1);
  TEST_VRSRA_N(, uint, u, 16, 4, 1);
  TEST_VRSRA_N(, uint, u, 32, 2, 1);
  TEST_VRSRA_N(, uint, u, 64, 1, 1);
  TEST_VRSRA_N(q, int, s, 8, 16, 1);
  TEST_VRSRA_N(q, int, s, 16, 8, 1);
  TEST_VRSRA_N(q, int, s, 32, 4, 1);
  TEST_VRSRA_N(q, int, s, 64, 2, 1);
  TEST_VRSRA_N(q, uint, u, 8, 16, 1);
  TEST_VRSRA_N(q, uint, u, 16, 8, 1);
  TEST_VRSRA_N(q, uint, u, 32, 4, 1);
  TEST_VRSRA_N(q, uint, u, 64, 2, 1);

#undef CMT
#define CMT " (checking overflow: shift by 1, max input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_sh1, CMT);


  /* Shift by 3 to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 3);
  TEST_VRSRA_N(, int, s, 16, 4, 3);
  TEST_VRSRA_N(, int, s, 32, 2, 3);
  TEST_VRSRA_N(, int, s, 64, 1, 3);
  TEST_VRSRA_N(, uint, u, 8, 8, 3);
  TEST_VRSRA_N(, uint, u, 16, 4, 3);
  TEST_VRSRA_N(, uint, u, 32, 2, 3);
  TEST_VRSRA_N(, uint, u, 64, 1, 3);
  TEST_VRSRA_N(q, int, s, 8, 16, 3);
  TEST_VRSRA_N(q, int, s, 16, 8, 3);
  TEST_VRSRA_N(q, int, s, 32, 4, 3);
  TEST_VRSRA_N(q, int, s, 64, 2, 3);
  TEST_VRSRA_N(q, uint, u, 8, 16, 3);
  TEST_VRSRA_N(q, uint, u, 16, 8, 3);
  TEST_VRSRA_N(q, uint, u, 32, 4, 3);
  TEST_VRSRA_N(q, uint, u, 64, 2, 3);

#undef CMT
#define CMT " (checking overflow: shift by 3, max input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_sh3, CMT);


  /* Shift by max to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 8);
  TEST_VRSRA_N(, int, s, 16, 4, 16);
  TEST_VRSRA_N(, int, s, 32, 2, 32);
  TEST_VRSRA_N(, int, s, 64, 1, 64);
  TEST_VRSRA_N(, uint, u, 8, 8, 8);
  TEST_VRSRA_N(, uint, u, 16, 4, 16);
  TEST_VRSRA_N(, uint, u, 32, 2, 32);
  TEST_VRSRA_N(, uint, u, 64, 1, 64);
  TEST_VRSRA_N(q, int, s, 8, 16, 8);
  TEST_VRSRA_N(q, int, s, 16, 8, 16);
  TEST_VRSRA_N(q, int, s, 32, 4, 32);
  TEST_VRSRA_N(q, int, s, 64, 2, 64);
  TEST_VRSRA_N(q, uint, u, 8, 16, 8);
  TEST_VRSRA_N(q, uint, u, 16, 8, 16);
  TEST_VRSRA_N(q, uint, u, 32, 4, 32);
  TEST_VRSRA_N(q, uint, u, 64, 2, 64);

#undef CMT
#define CMT " (checking overflow: shift by max, max input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max_shmax, CMT);


  /* Initialize with min values to check overflow.  */
  VDUP(vector2, , int, s, 8, 8, 0x80);
  VDUP(vector2, , int, s, 16, 4, 0x8000);
  VDUP(vector2, , int, s, 32, 2, 0x80000000);
  VDUP(vector2, , int, s, 64, 1, 0x8000000000000000LL);
  VDUP(vector2, q, int, s, 8, 16, 0x80);
  VDUP(vector2, q, int, s, 16, 8, 0x8000);
  VDUP(vector2, q, int, s, 32, 4, 0x80000000);
  VDUP(vector2, q, int, s, 64, 2, 0x8000000000000000ULL);

  /* Shift by 1 to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 1);
  TEST_VRSRA_N(, int, s, 16, 4, 1);
  TEST_VRSRA_N(, int, s, 32, 2, 1);
  TEST_VRSRA_N(, int, s, 64, 1, 1);
  TEST_VRSRA_N(q, int, s, 8, 16, 1);
  TEST_VRSRA_N(q, int, s, 16, 8, 1);
  TEST_VRSRA_N(q, int, s, 32, 4, 1);
  TEST_VRSRA_N(q, int, s, 64, 2, 1);

#undef CMT
#define CMT " (checking overflow: shift by 1, min negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_min_sh1, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_min_sh1, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_min_sh1, CMT);


  /* Shift by 3 to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 3);
  TEST_VRSRA_N(, int, s, 16, 4, 3);
  TEST_VRSRA_N(, int, s, 32, 2, 3);
  TEST_VRSRA_N(, int, s, 64, 1, 3);
  TEST_VRSRA_N(q, int, s, 8, 16, 3);
  TEST_VRSRA_N(q, int, s, 16, 8, 3);
  TEST_VRSRA_N(q, int, s, 32, 4, 3);
  TEST_VRSRA_N(q, int, s, 64, 2, 3);

#undef CMT
#define CMT " (checking overflow: shift by 3, min negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_min_sh3, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_min_sh3, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_min_sh3, CMT);


  /* Shift by max to check overflow with rounding constant.  */
  TEST_VRSRA_N(, int, s, 8, 8, 8);
  TEST_VRSRA_N(, int, s, 16, 4, 16);
  TEST_VRSRA_N(, int, s, 32, 2, 32);
  TEST_VRSRA_N(, int, s, 64, 1, 64);
  TEST_VRSRA_N(q, int, s, 8, 16, 8);
  TEST_VRSRA_N(q, int, s, 16, 8, 16);
  TEST_VRSRA_N(q, int, s, 32, 4, 32);
  TEST_VRSRA_N(q, int, s, 64, 2, 64);

#undef CMT
#define CMT " (checking overflow: shift by max, min negative input)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_min_shmax, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_min_shmax, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_min_shmax, CMT);
}

int main (void)
{
  exec_vrsra_n ();
  return 0;
}

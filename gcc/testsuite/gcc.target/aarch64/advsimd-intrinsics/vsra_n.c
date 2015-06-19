#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
				       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffd };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x5, 0x6, 0x7, 0x8,
					0x9, 0xa, 0xb, 0xc };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfffc, 0xfffd, 0xfffe, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff3, 0xfffffff4 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					0xfc, 0xfd, 0xfe, 0xff,
					0x0, 0x1, 0x2, 0x3,
					0x4, 0x5, 0x6, 0x7 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffc, 0xfffffffd,
					0xfffffffe, 0xffffffff };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0,
					0xfffffffffffffff1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x5, 0x6, 0x7, 0x8,
					 0x9, 0xa, 0xb, 0xc,
					 0xd, 0xe, 0xf, 0x10,
					 0x11, 0x12, 0x13, 0x14 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfffc, 0xfffd, 0xfffe, 0xffff,
					 0x0, 0x1, 0x2, 0x3 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff3, 0xfffffff4,
					 0xfffffff5, 0xfffffff6 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff1 };

#define TEST_MSG "VSRA_N"
void exec_vsra_n (void)
{
  /* Basic test: y=vsra_n(x,v), then store the result.  */
#define TEST_VSRA_N(Q, T1, T2, W, N, V)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vsra##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
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
  TEST_VSRA_N(, int, s, 8, 8, 1);
  TEST_VSRA_N(, int, s, 16, 4, 12);
  TEST_VSRA_N(, int, s, 32, 2, 2);
  TEST_VSRA_N(, int, s, 64, 1, 32);
  TEST_VSRA_N(, uint, u, 8, 8, 2);
  TEST_VSRA_N(, uint, u, 16, 4, 3);
  TEST_VSRA_N(, uint, u, 32, 2, 5);
  TEST_VSRA_N(, uint, u, 64, 1, 33);

  TEST_VSRA_N(q, int, s, 8, 16, 1);
  TEST_VSRA_N(q, int, s, 16, 8, 12);
  TEST_VSRA_N(q, int, s, 32, 4, 2);
  TEST_VSRA_N(q, int, s, 64, 2, 32);
  TEST_VSRA_N(q, uint, u, 8, 16, 2);
  TEST_VSRA_N(q, uint, u, 16, 8, 3);
  TEST_VSRA_N(q, uint, u, 32, 4, 5);
  TEST_VSRA_N(q, uint, u, 64, 2, 33);

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
}

int main (void)
{
  exec_vsra_n ();
  return 0;
}

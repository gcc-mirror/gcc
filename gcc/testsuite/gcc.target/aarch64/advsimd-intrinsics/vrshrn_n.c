#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with input=0.  */
VECT_VAR_DECL(expected_0,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
					  0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0,uint,32,2) [] = { 0x0, 0x0 };

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf9, 0xf9, 0xfa,
				       0xfa, 0xfb, 0xfb, 0xfc };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff8, 0xfff9, 0xfff9, 0xfffa };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xfc, 0xfc, 0xfd, 0xfd,
					0xfd, 0xfd, 0xfe, 0xfe };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffffe, 0xfffffffe };

/* Expected results with large shift amount.  */
VECT_VAR_DECL(expected_sh_large,int,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large,int,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large,int,32,2) [] = { 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large,uint,8,8) [] = { 0x0, 0x0, 0x0, 0x0,
						 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large,uint,16,4) [] = { 0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large,uint,32,2) [] = { 0x0, 0x0 };

#define TEST_MSG "VRSHRN_N"
void exec_vrshrn_n (void)
{
  /* Basic test: v2=vrshrn_n(v1,v), then store the result.  */
#define TEST_VRSHRN_N(T1, T2, W, N, W2, V)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vrshrn_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
		     V);						\
  vst1_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

  /* vector is twice as large as vector_res.  */
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);

  DECL_VARIABLE(vector_res, int, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 4);
  DECL_VARIABLE(vector_res, int, 32, 2);
  DECL_VARIABLE(vector_res, uint, 8, 8);
  DECL_VARIABLE(vector_res, uint, 16, 4);
  DECL_VARIABLE(vector_res, uint, 32, 2);

  clean_results ();

  /* Fill input vector with 0, to check behavior on limits.  */
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 16, 8, 0);
  VDUP(vector, q, uint, u, 32, 4, 0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSHRN_N(int, s, 16, 8, 8, 1);
  TEST_VRSHRN_N(int, s, 32, 4, 16, 1);
  TEST_VRSHRN_N(int, s, 64, 2, 32, 2);
  TEST_VRSHRN_N(uint, u, 16, 8, 8, 2);
  TEST_VRSHRN_N(uint, u, 32, 4, 16, 3);
  TEST_VRSHRN_N(uint, u, 64, 2, 32, 3);

#define CMT " (with input = 0)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_0, CMT);


  /* Test again, with predefined input values.  */
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSHRN_N(int, s, 16, 8, 8, 1);
  TEST_VRSHRN_N(int, s, 32, 4, 16, 1);
  TEST_VRSHRN_N(int, s, 64, 2, 32, 2);
  TEST_VRSHRN_N(uint, u, 16, 8, 8, 2);
  TEST_VRSHRN_N(uint, u, 32, 4, 16, 3);
  TEST_VRSHRN_N(uint, u, 64, 2, 32, 3);

#undef CMT
#define CMT ""
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);


  /* Fill input arbitrary values.  */
  VDUP(vector, q, int, s, 16, 8, 30);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 16, 8, 0xFFF0);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFF0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Choose large shift amount arbitrarily.  */
  TEST_VRSHRN_N(int, s, 16, 8, 8, 7);
  TEST_VRSHRN_N(int, s, 32, 4, 16, 14);
  TEST_VRSHRN_N(int, s, 64, 2, 32, 31);
  TEST_VRSHRN_N(uint, u, 16, 8, 8, 7);
  TEST_VRSHRN_N(uint, u, 32, 4, 16, 16);
  TEST_VRSHRN_N(uint, u, 64, 2, 32, 3);

#undef CMT
#define CMT " (with large shift amount)"
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_sh_large, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_sh_large, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_sh_large, CMT);
}

int main (void)
{
  exec_vrshrn_n ();
  return 0;
}

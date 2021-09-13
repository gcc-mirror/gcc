/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results with input=0.  */
VECT_VAR_DECL(expected_0, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					     0x5, 0x5, 0x5, 0x5,
					     0x0, 0x0, 0x0, 0x0,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0, int, 32, 4) [] = { 0x5, 0x5, 0x0, 0x0 };
VECT_VAR_DECL(expected_0, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					      0x5, 0x5, 0x5, 0x5,
					      0x0, 0x0, 0x0, 0x0,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					      0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_0, uint, 32, 4) [] = { 0x5, 0x5, 0x0, 0x0 };

/* Expected results.  */
VECT_VAR_DECL(expected, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					   0x5, 0x5, 0x5, 0x5,
					   0xf8, 0xf9, 0xf9, 0xfa,
					   0xfa, 0xfb, 0xfb, 0xfc };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					   0xfff8, 0xfff9, 0xfff9, 0xfffa };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x5, 0x5, 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0xfc, 0xfc, 0xfd, 0xfd,
					    0xfd, 0xfd, 0xfe, 0xfe };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x5, 0x5, 0xfffffffe, 0xfffffffe };

/* Expected results with large shift amount.  */
VECT_VAR_DECL(expected_sh_large, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
						    0x5, 0x5, 0x5, 0x5,
						    0x0, 0x0, 0x0, 0x0,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
						    0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large, int, 32, 4) [] = { 0x5, 0x5, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
						     0x5, 0x5, 0x5, 0x5,
						     0x0, 0x0, 0x0, 0x0,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
						     0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_sh_large, uint, 32, 4) [] = { 0x5, 0x5, 0x0, 0x0 };

#define TEST_MSG "VRSHRN_HIGH_N"
void exec_vrshrn_high_n (void)
{
  /* Basic test: y=vrshrn_high_n(r,x,v), then store the result.  */
#define TEST_VRSHRN_HIGH_N(T1, T2, W1, W2, N1, N2, V)				\
  VECT_VAR(vector_res, T1, W2, N2) =						\
    vrshrn_high_n_##T2##W1(VECT_VAR(vector_res_lo, T1, W2, N1),			\
			   VECT_VAR(vector, T1, W1, N1),			\
			   V);							\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2), VECT_VAR(vector_res, T1, W2, N2))

  DECL_VARIABLE(vector_res_lo, int, 8, 8);
  DECL_VARIABLE(vector_res_lo, int, 16, 4);
  DECL_VARIABLE(vector_res_lo, int, 32, 2);
  DECL_VARIABLE(vector_res_lo, uint, 8, 8);
  DECL_VARIABLE(vector_res_lo, uint, 16, 4);
  DECL_VARIABLE(vector_res_lo, uint, 32, 2);

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, int, 64, 2);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, uint, 64, 2);

  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  clean_results ();

  /* Fill vector_res_lo with a value easy to recognise in the result vector. */
  VDUP(vector_res_lo, , int, s, 8, 8, 0x5);
  VDUP(vector_res_lo, , int, s, 16, 4, 0x5);
  VDUP(vector_res_lo, , int, s, 32, 2, 0x5);
  VDUP(vector_res_lo, , uint, u, 8, 8, 0x5);
  VDUP(vector_res_lo, , uint, u, 16, 4, 0x5);
  VDUP(vector_res_lo, , uint, u, 32, 2, 0x5);

  /* Fill input vector with 0, to check behavior on limits.  */
  VDUP(vector, q, int, s, 16, 8, 0);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 16, 8, 0);
  VDUP(vector, q, uint, u, 32, 4, 0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSHRN_HIGH_N(int, s, 16, 8, 8, 16, 1);
  TEST_VRSHRN_HIGH_N(int, s, 32, 16, 4, 8, 1);
  TEST_VRSHRN_HIGH_N(int, s, 64, 32, 2, 4, 2);
  TEST_VRSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 2);
  TEST_VRSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 3);
  TEST_VRSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

#define CMT " (with input = 0)"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_0, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_0, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_0, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_0, CMT);


  /* Test again, with predefined input values.  */
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Choose shift amount arbitrarily.  */
  TEST_VRSHRN_HIGH_N(int, s, 16, 8, 8, 16, 1);
  TEST_VRSHRN_HIGH_N(int, s, 32, 16, 4, 8, 1);
  TEST_VRSHRN_HIGH_N(int, s, 64, 32, 2, 4, 2);
  TEST_VRSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 2);
  TEST_VRSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 3);
  TEST_VRSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

#undef CMT
#define CMT ""
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);

  /* Fill input arbitrary values.  */
  VDUP(vector, q, int, s, 16, 8, 30);
  VDUP(vector, q, int, s, 32, 4, 0);
  VDUP(vector, q, int, s, 64, 2, 0);
  VDUP(vector, q, uint, u, 16, 8, 0xFFF0);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFF0);
  VDUP(vector, q, uint, u, 64, 2, 0);

  /* Choose large shift amount arbitrarily.  */
  TEST_VRSHRN_HIGH_N(int, s, 16, 8, 8, 16, 7);
  TEST_VRSHRN_HIGH_N(int, s, 32, 16, 4, 8, 14);
  TEST_VRSHRN_HIGH_N(int, s, 64, 32, 2, 4, 31);
  TEST_VRSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 7);
  TEST_VRSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 16);
  TEST_VRSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

#undef CMT
#define CMT " (with large shift amount)"
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_sh_large, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_sh_large, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_sh_large, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_sh_large, CMT);
}

int main (void)
{
  exec_vrshrn_high_n ();
  return 0;
}

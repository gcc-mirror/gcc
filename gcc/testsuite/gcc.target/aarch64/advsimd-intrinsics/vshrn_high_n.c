/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					   0x5, 0x5, 0x5, 0x5,
					   0xf8, 0xf8, 0xf9, 0xf9,
					   0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					   0xfff8, 0xfff8, 0xfff9, 0xfff9 };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x5, 0x5, 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0xfc, 0xfc, 0xfc, 0xfc,
					    0xfd, 0xfd, 0xfd, 0xfd };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x5, 0x5, 0xfffffffe, 0xfffffffe };

#define TEST_MSG "VSHRN_HIGH_N"
void exec_vshrn_high_n (void)
{
  /* Basic test: y=vshrn_high_n(r,x,v), then store the result.  */
#define TEST_VSHRN_HIGH_N(T1, T2, W1, W2, N1, N2, V)				\
  VECT_VAR(vector_res, T1, W2, N2) =						\
    vshrn_high_n_##T2##W1(VECT_VAR(vector_res_lo, T1, W2, N1),			\
			  VECT_VAR(vector, T1, W1, N1),				\
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

  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Choose shift amount arbitrarily.  */
  TEST_VSHRN_HIGH_N(int, s, 16, 8, 8, 16, 1);
  TEST_VSHRN_HIGH_N(int, s, 32, 16, 4, 8, 1);
  TEST_VSHRN_HIGH_N(int, s, 64, 32, 2, 4, 2);
  TEST_VSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 2);
  TEST_VSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 3);
  TEST_VSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

#define CMT ""
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);
}

int main (void)
{
  exec_vshrn_high_n ();
  return 0;
}

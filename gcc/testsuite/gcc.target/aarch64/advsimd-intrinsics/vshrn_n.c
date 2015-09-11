#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
				       0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff8, 0xfff8, 0xfff9, 0xfff9 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xfc, 0xfc, 0xfc, 0xfc,
					0xfd, 0xfd, 0xfd, 0xfd };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfffe, 0xfffe, 0xfffe, 0xfffe };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffffe, 0xfffffffe };

#define TEST_MSG "VSHRN_N"
void exec_vshrn_n (void)
{
  /* Basic test: y=vshrn_n(x,v), then store the result.  */
#define TEST_VSHRN_N(T1, T2, W, W2, N, V)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vshrn_n_##T2##W(VECT_VAR(vector, T1, W, N),				\
		    V);							\
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

  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, int, s, 64, 2);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 64, 2);

  /* Choose shift amount arbitrarily.  */
  TEST_VSHRN_N(int, s, 16, 8, 8, 1);
  TEST_VSHRN_N(int, s, 32, 16, 4, 1);
  TEST_VSHRN_N(int, s, 64, 32, 2, 2);
  TEST_VSHRN_N(uint, u, 16, 8, 8, 2);
  TEST_VSHRN_N(uint, u, 32, 16, 4, 3);
  TEST_VSHRN_N(uint, u, 64, 32, 2, 3);

#define CMT ""
  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);
}

int main (void)
{
  exec_vshrn_n ();
  return 0;
}

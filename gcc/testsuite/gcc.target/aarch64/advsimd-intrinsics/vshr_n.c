#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
				       0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0x3c, 0x3c, 0x3c, 0x3c,
					0x3d, 0x3d, 0x3d, 0x3d };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0x1ffe, 0x1ffe, 0x1ffe, 0x1ffe };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x7ffffff, 0x7ffffff };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x7fffffff };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
					0xfa, 0xfa, 0xfb, 0xfb,
					0xfc, 0xfc, 0xfd, 0xfd,
					0xfe, 0xfe, 0xff, 0xff };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffffc, 0xfffffffc,
					0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffff, 0xffffffffffffffff };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x3c, 0x3c, 0x3c, 0x3c,
					 0x3d, 0x3d, 0x3d, 0x3d,
					 0x3e, 0x3e, 0x3e, 0x3e,
					 0x3f, 0x3f, 0x3f, 0x3f };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x1ffe, 0x1ffe, 0x1ffe, 0x1ffe,
					 0x1ffe, 0x1ffe, 0x1ffe, 0x1ffe };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x7ffffff, 0x7ffffff,
					 0x7ffffff, 0x7ffffff };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x7fffffff, 0x7fffffff };

#define TEST_MSG "VSHR_N"
void exec_vshr_n (void)
{
  /* Basic test: y=vshr_n(x,v), then store the result.  */
#define TEST_VSHR_N(Q, T1, T2, W, N, V)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vshr##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose shift amount arbitrarily.  */
  TEST_VSHR_N(, int, s, 8, 8, 1);
  TEST_VSHR_N(, int, s, 16, 4, 12);
  TEST_VSHR_N(, int, s, 32, 2, 2);
  TEST_VSHR_N(, int, s, 64, 1, 32);
  TEST_VSHR_N(, uint, u, 8, 8, 2);
  TEST_VSHR_N(, uint, u, 16, 4, 3);
  TEST_VSHR_N(, uint, u, 32, 2, 5);
  TEST_VSHR_N(, uint, u, 64, 1, 33);

  TEST_VSHR_N(q, int, s, 8, 16, 1);
  TEST_VSHR_N(q, int, s, 16, 8, 12);
  TEST_VSHR_N(q, int, s, 32, 4, 2);
  TEST_VSHR_N(q, int, s, 64, 2, 32);
  TEST_VSHR_N(q, uint, u, 8, 16, 2);
  TEST_VSHR_N(q, uint, u, 16, 8, 3);
  TEST_VSHR_N(q, uint, u, 32, 4, 5);
  TEST_VSHR_N(q, uint, u, 64, 2, 33);

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
}

int main (void)
{
  exec_vshr_n ();
  return 0;
}

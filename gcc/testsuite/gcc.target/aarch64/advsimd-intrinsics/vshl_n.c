#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xe0, 0xe2, 0xe4, 0xe6,
				       0xe8, 0xea, 0xec, 0xee };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffe0, 0xffe2, 0xffe4, 0xffe6 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffff80, 0xffffff88 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffc0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xc0, 0xc4, 0xc8, 0xcc,
					0xd0, 0xd4, 0xd8, 0xdc };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xff00, 0xff10, 0xff20, 0xff30 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffff80, 0xffffff88 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffffe0 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x0, 0x20, 0x40, 0x60,
					0x80, 0xa0, 0xc0, 0xe0,
					0x0, 0x20, 0x40, 0x60,
					0x80, 0xa0, 0xc0, 0xe0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffe0, 0xffe2, 0xffe4, 0xffe6,
					0xffe8, 0xffea, 0xffec, 0xffee };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffc0, 0xffffffc4,
					0xffffffc8, 0xffffffcc };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffc0, 0xffffffffffffffc4 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xc0, 0xc4, 0xc8, 0xcc,
					 0xd0, 0xd4, 0xd8, 0xdc,
					 0xe0, 0xe4, 0xe8, 0xec,
					 0xf0, 0xf4, 0xf8, 0xfc };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xff80, 0xff88, 0xff90, 0xff98,
					 0xffa0, 0xffa8, 0xffb0, 0xffb8 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffc0, 0xffffffc4,
					 0xffffffc8, 0xffffffcc };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffffe0,
					 0xffffffffffffffe2 };

#define TEST_MSG "VSHL_N"
void exec_vshl_n (void)
{
  /* Basic test: v2=vshl_n(v1,v), then store the result.  */
#define TEST_VSHL_N(Q, T1, T2, W, N, V)					\
  VECT_VAR(vector_res, T1, W, N) =					\
    vshl##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose shift amount arbitrarily.  */
  TEST_VSHL_N(, int, s, 8, 8, 1);
  TEST_VSHL_N(, int, s, 16, 4, 1);
  TEST_VSHL_N(, int, s, 32, 2, 3);
  TEST_VSHL_N(, int, s, 64, 1, 2);
  TEST_VSHL_N(, uint, u, 8, 8, 2);
  TEST_VSHL_N(, uint, u, 16, 4, 4);
  TEST_VSHL_N(, uint, u, 32, 2, 3);
  TEST_VSHL_N(, uint, u, 64, 1, 1);

  TEST_VSHL_N(q, int, s, 8, 16, 5);
  TEST_VSHL_N(q, int, s, 16, 8, 1);
  TEST_VSHL_N(q, int, s, 32, 4, 2);
  TEST_VSHL_N(q, int, s, 64, 2, 2);
  TEST_VSHL_N(q, uint, u, 8, 16, 2);
  TEST_VSHL_N(q, uint, u, 16, 8, 3);
  TEST_VSHL_N(q, uint, u, 32, 4, 2);
  TEST_VSHL_N(q, uint, u, 64, 2, 1);

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
  exec_vshl_n ();
  return 0;
}

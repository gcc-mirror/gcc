/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7,
					0xf8, 0xf8, 0xf9, 0xf9,
					0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff8, 0xfff8, 0xfff9, 0xfff9 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					 0xffffffff, 0xffffffff };

/* Expected results with max input value shifted by 3.  */
VECT_VAR_DECL(expected_max_sh3,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f,
						0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_sh3,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
						0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_sh3,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
						0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_sh3,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff,
						 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_sh3,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
						 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_sh3,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						 0xffffffff, 0xffffffff };

/* Expected results with max input value shifted by type size.  */
VECT_VAR_DECL(expected_max_shmax,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						  0x7f, 0x7f, 0x7f, 0x7f,
						  0x7f, 0x7f, 0x7f, 0x7f,
						  0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_shmax,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
						  0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_shmax,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
						  0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_shmax,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff,
						   0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_shmax,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
						   0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_shmax,uint,32,4) [] = { 0xffffffff, 0xffffffff,
						   0xffffffff, 0xffffffff };

#define INSN vqshrn_high_n
#define TEST_MSG "VQSHRN_HIGH_N"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: y=vqshrn_high_n(x1,x2,v), then store the result.  */
#define TEST_VQSHRN_HIGH_N2(INSN, T1, T2, W, W2, N, N2, V) \
  VECT_VAR(vector_res, T1, W2, N2) =			   \
    INSN##_##T2##W(VECT_VAR(vector1, T1, W2, N),	   \
		   VECT_VAR(vector2, T1, W, N), V);	   \
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2),		   \
		 VECT_VAR(vector_res, T1, W2, N2));

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHRN_HIGH_N1(INSN, T1, T2, W, W2, N, N2, V) \
  TEST_VQSHRN_HIGH_N2(INSN, T1, T2, W, W2, N, N2, V)

#define TEST_VQSHRN_HIGH_N(T1, T2, W, W2, N, N2, V) \
  TEST_VQSHRN_HIGH_N1(INSN, T1, T2, W, W2, N, N2, V)


  DECL_VARIABLE(vector1, int, 8, 8);
  DECL_VARIABLE(vector1, int, 16, 4);
  DECL_VARIABLE(vector1, int, 32, 2);
  DECL_VARIABLE(vector1, uint, 8, 8);
  DECL_VARIABLE(vector1, uint, 16, 4);
  DECL_VARIABLE(vector1, uint, 32, 2);

  /* vector is twice as large as vector_res.  */
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);
  DECL_VARIABLE(vector2, int, 64, 2);
  DECL_VARIABLE(vector2, uint, 16, 8);
  DECL_VARIABLE(vector2, uint, 32, 4);
  DECL_VARIABLE(vector2, uint, 64, 2);

  DECL_VARIABLE(vector_res, int, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, uint, 8, 16);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);

  clean_results ();

  VLOAD(vector1, buffer, , int, s, 8, 8);
  VLOAD(vector1, buffer, , int, s, 16, 4);
  VLOAD(vector1, buffer, , int, s, 32, 2);
  VLOAD(vector1, buffer, , uint, u, 8, 8);
  VLOAD(vector1, buffer, , uint, u, 16, 4);
  VLOAD(vector1, buffer, , uint, u, 32, 2);

  VLOAD(vector2, buffer, q, int, s, 16, 8);
  VLOAD(vector2, buffer, q, int, s, 32, 4);
  VLOAD(vector2, buffer, q, int, s, 64, 2);
  VLOAD(vector2, buffer, q, uint, u, 16, 8);
  VLOAD(vector2, buffer, q, uint, u, 32, 4);
  VLOAD(vector2, buffer, q, uint, u, 64, 2);

  /* Choose shift amount arbitrarily.  */
#define CMT ""
  TEST_VQSHRN_HIGH_N(int, s, 16, 8, 8, 16, 1);
  TEST_VQSHRN_HIGH_N(int, s, 32, 16, 4, 8, 1);
  TEST_VQSHRN_HIGH_N(int, s, 64, 32, 2, 4, 2);
  TEST_VQSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 2);
  TEST_VQSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 3);
  TEST_VQSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);

  /* Use max possible value as input.  */
  VDUP(vector1, , int, s, 8, 8, 0x7F);
  VDUP(vector1, , int, s, 16, 4, 0x7FFF);
  VDUP(vector1, , int, s, 32, 2, 0x7FFFFFFFLL);
  VDUP(vector1, , uint, u, 8, 8, 0xFF);
  VDUP(vector1, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector1, , uint, u, 32, 2, 0xFFFFFFFFULL);

  VDUP(vector2, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector2, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector2, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector2, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector2, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector2, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

#undef CMT
#define CMT " (check saturation: shift by 3)"
  TEST_VQSHRN_HIGH_N(int, s, 16, 8, 8, 16, 3);
  TEST_VQSHRN_HIGH_N(int, s, 32, 16, 4, 8, 3);
  TEST_VQSHRN_HIGH_N(int, s, 64, 32, 2, 4, 3);
  TEST_VQSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 3);
  TEST_VQSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 3);
  TEST_VQSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 3);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_sh3, CMT);


#undef CMT
#define CMT " (check saturation: shift by max)"
  TEST_VQSHRN_HIGH_N(int, s, 16, 8, 8, 16, 8);
  TEST_VQSHRN_HIGH_N(int, s, 32, 16, 4, 8, 16);
  TEST_VQSHRN_HIGH_N(int, s, 64, 32, 2, 4, 32);
  TEST_VQSHRN_HIGH_N(uint, u, 16, 8, 8, 16, 8);
  TEST_VQSHRN_HIGH_N(uint, u, 32, 16, 4, 8, 16);
  TEST_VQSHRN_HIGH_N(uint, u, 64, 32, 2, 4, 32);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max_shmax, CMT);
}

int main (void)
{
  exec_vqshrn_high_n ();
  return 0;
}

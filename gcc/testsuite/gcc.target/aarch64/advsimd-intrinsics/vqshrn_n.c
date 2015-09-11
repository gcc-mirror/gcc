#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat,uint,16,8) = 1;
int VECT_VAR(expected_cumulative_sat,uint,32,4) = 1;
int VECT_VAR(expected_cumulative_sat,uint,64,2) = 1;

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf8, 0xf9, 0xf9,
				       0xfa, 0xfa, 0xfb, 0xfb };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff8, 0xfff8, 0xfff9, 0xfff9 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffffc, 0xfffffffc };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };

/* Expected values of cumulative_saturation flag with max input value
   shifted by 3.  */
int VECT_VAR(expected_cumulative_sat_max_sh3,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh3,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh3,int,64,2) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh3,uint,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh3,uint,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_max_sh3,uint,64,2) = 1;

/* Expected results with max input value shifted by 3.  */
VECT_VAR_DECL(expected_max_sh3,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					       0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_sh3,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_sh3,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_sh3,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_sh3,uint,16,4) [] = { 0xffff, 0xffff,
						 0xffff, 0xffff };
VECT_VAR_DECL(expected_max_sh3,uint,32,2) [] = { 0xffffffff, 0xffffffff };

/* Expected values of cumulative_saturation flag with max input value
   shifted by type size.  */
int VECT_VAR(expected_cumulative_sat_max_shmax,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat_max_shmax,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat_max_shmax,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat_max_shmax,uint,16,8) = 0;
int VECT_VAR(expected_cumulative_sat_max_shmax,uint,32,4) = 0;
int VECT_VAR(expected_cumulative_sat_max_shmax,uint,64,2) = 0;

/* Expected results with max input value shifted by type size.  */
VECT_VAR_DECL(expected_max_shmax,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
						 0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max_shmax,int,16,4) [] = { 0x7fff, 0x7fff,
						  0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max_shmax,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max_shmax,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
						  0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max_shmax,uint,16,4) [] = { 0xffff, 0xffff,
						   0xffff, 0xffff };
VECT_VAR_DECL(expected_max_shmax,uint,32,2) [] = { 0xffffffff, 0xffffffff };

#define INSN vqshrn_n
#define TEST_MSG "VQSHRN_N"

#define FNNAME1(NAME) void exec_ ## NAME (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: y=vqshrn_n(x,v), then store the result.  */
#define TEST_VQSHRN_N2(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W2, N));		\
  VECT_VAR(vector_res, T1, W2, N) =					\
    INSN##_##T2##W(VECT_VAR(vector, T1, W, N),				\
		   V);							\
  vst1_##T2##W2(VECT_VAR(result, T1, W2, N),				\
		VECT_VAR(vector_res, T1, W2, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHRN_N1(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQSHRN_N2(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQSHRN_N(T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQSHRN_N1(INSN, T1, T2, W, W2, N, V, EXPECTED_CUMULATIVE_SAT, CMT)


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
#define CMT ""
  TEST_VQSHRN_N(int, s, 16, 8, 8, 1, expected_cumulative_sat, CMT);
  TEST_VQSHRN_N(int, s, 32, 16, 4, 1, expected_cumulative_sat, CMT);
  TEST_VQSHRN_N(int, s, 64, 32, 2, 2, expected_cumulative_sat, CMT);
  TEST_VQSHRN_N(uint, u, 16, 8, 8, 2, expected_cumulative_sat, CMT);
  TEST_VQSHRN_N(uint, u, 32, 16, 4, 3, expected_cumulative_sat, CMT);
  TEST_VQSHRN_N(uint, u, 64, 32, 2, 3, expected_cumulative_sat, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, CMT);


  /* Use max possible value as input.  */
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

#undef CMT
#define CMT " (check saturation: shift by 3)"
  TEST_VQSHRN_N(int, s, 16, 8, 8, 3, expected_cumulative_sat_max_sh3, CMT);
  TEST_VQSHRN_N(int, s, 32, 16, 4, 3, expected_cumulative_sat_max_sh3, CMT);
  TEST_VQSHRN_N(int, s, 64, 32, 2, 3, expected_cumulative_sat_max_sh3, CMT);
  TEST_VQSHRN_N(uint, u, 16, 8, 8, 3, expected_cumulative_sat_max_sh3, CMT);
  TEST_VQSHRN_N(uint, u, 32, 16, 4, 3, expected_cumulative_sat_max_sh3, CMT);
  TEST_VQSHRN_N(uint, u, 64, 32, 2, 3, expected_cumulative_sat_max_sh3, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_sh3, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_sh3, CMT);


#undef CMT
#define CMT " (check saturation: shift by max)"
  TEST_VQSHRN_N(int, s, 16, 8, 8, 8, expected_cumulative_sat_max_shmax, CMT);
  TEST_VQSHRN_N(int, s, 32, 16, 4, 16, expected_cumulative_sat_max_shmax, CMT);
  TEST_VQSHRN_N(int, s, 64, 32, 2, 32, expected_cumulative_sat_max_shmax, CMT);
  TEST_VQSHRN_N(uint, u, 16, 8, 8, 8, expected_cumulative_sat_max_shmax, CMT);
  TEST_VQSHRN_N(uint, u, 32, 16, 4, 16, expected_cumulative_sat_max_shmax, CMT);
  TEST_VQSHRN_N(uint, u, 64, 32, 2, 32, expected_cumulative_sat_max_shmax, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max_shmax, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max_shmax, CMT);
}

int main (void)
{
  exec_vqshrn_n ();
  return 0;
}

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected values of cumulative_saturation flag.  */
int VECT_VAR(expected_cumulative_sat,int,8,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,2) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,1) = 0;
int VECT_VAR(expected_cumulative_sat,uint,8,8) = 1;
int VECT_VAR(expected_cumulative_sat,uint,16,4) = 1;
int VECT_VAR(expected_cumulative_sat,uint,32,2) = 1;
int VECT_VAR(expected_cumulative_sat,uint,64,1) = 1;
int VECT_VAR(expected_cumulative_sat,int,8,16) = 0;
int VECT_VAR(expected_cumulative_sat,int,16,8) = 0;
int VECT_VAR(expected_cumulative_sat,int,32,4) = 0;
int VECT_VAR(expected_cumulative_sat,int,64,2) = 0;
int VECT_VAR(expected_cumulative_sat,uint,8,16) = 1;
int VECT_VAR(expected_cumulative_sat,uint,16,8) = 1;
int VECT_VAR(expected_cumulative_sat,uint,32,4) = 1;
int VECT_VAR(expected_cumulative_sat,uint,64,2) = 1;

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xc0, 0xc4, 0xc8, 0xcc,
				       0xd0, 0xd4, 0xd8, 0xdc };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xffe0, 0xffe2, 0xffe4, 0xffe6 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xffffffe0, 0xffffffe2 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xffffffffffffffc0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xc0, 0xc4, 0xc8, 0xcc,
					0xd0, 0xd4, 0xd8, 0xdc,
					0xe0, 0xe4, 0xe8, 0xec,
					0xf0, 0xf4, 0xf8, 0xfc };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xffe0, 0xffe2, 0xffe4, 0xffe6,
					0xffe8, 0xffea, 0xffec, 0xffee };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xffffffe0, 0xffffffe2,
					0xffffffe4, 0xffffffe6 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xffffffffffffffc0, 0xffffffffffffffc4 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffffffff, 0xffffffff,
					 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffffffffffff,
					 0xffffffffffffffff };

/* Expected values of cumulative_saturation flag with max positive input.  */
int VECT_VAR(expected_cumulative_sat_max,int,8,8) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,16,4) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,32,2) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,64,1) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,8,8) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,16,4) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,32,2) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,64,1) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,8,16) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_max,int,64,2) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,8,16) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,16,8) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,32,4) = 1;
int VECT_VAR(expected_cumulative_sat_max,uint,64,2) = 1;

/* Expected results with max positive input.  */
VECT_VAR_DECL(expected_max,int,8,8) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					   0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max,int,16,4) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max,int,32,2) [] = { 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max,int,64,1) [] = { 0x7fffffffffffffff };
VECT_VAR_DECL(expected_max,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					    0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max,uint,16,4) [] = { 0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_max,uint,32,2) [] = { 0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max,uint,64,1) [] = { 0xffffffffffffffff };
VECT_VAR_DECL(expected_max,int,8,16) [] = { 0x7f, 0x7f, 0x7f, 0x7f,
					    0x7f, 0x7f, 0x7f, 0x7f,
					    0x7f, 0x7f, 0x7f, 0x7f,
					    0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected_max,int,16,8) [] = { 0x7fff, 0x7fff, 0x7fff, 0x7fff,
					    0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_max,int,32,4) [] = { 0x7fffffff, 0x7fffffff,
					    0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_max,int,64,2) [] = { 0x7fffffffffffffff,
					    0x7fffffffffffffff };
VECT_VAR_DECL(expected_max,uint,8,16) [] = { 0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected_max,uint,16,8) [] = { 0xffff, 0xffff, 0xffff, 0xffff,
					     0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected_max,uint,32,4) [] = { 0xffffffff, 0xffffffff,
					     0xffffffff, 0xffffffff };
VECT_VAR_DECL(expected_max,uint,64,2) [] = { 0xffffffffffffffff,
					     0xffffffffffffffff };

#define INSN vqshl
#define TEST_MSG "VQSHL_N/VQSHLQ_N"

#define FNNAME1(NAME) void exec_ ## NAME ##_n (void)
#define FNNAME(NAME) FNNAME1(NAME)

FNNAME (INSN)
{
  /* Basic test: v2=vqshl_n(v1,v), then store the result.  */
#define TEST_VQSHL_N2(INSN, Q, T1, T2, W, N, V, EXPECTED_CUMULATIVE_SAT, CMT) \
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vector_res, T1, W, N));		\
  VECT_VAR(vector_res, T1, W, N) =					\
    INSN##Q##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
			V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK_CUMULATIVE_SAT(TEST_MSG, T1, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  /* Two auxliary macros are necessary to expand INSN */
#define TEST_VQSHL_N1(INSN, T3, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT) \
  TEST_VQSHL_N2(INSN, T3, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

#define TEST_VQSHL_N(T3, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)	\
  TEST_VQSHL_N1(INSN, T3, Q, T1, T2, W, N, EXPECTED_CUMULATIVE_SAT, CMT)

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);

  /* Choose shift amount arbitrarily.  */
#define CMT ""
  TEST_VQSHL_N(, int, s, 8, 8, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, int, s, 16, 4, 1, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, int, s, 32, 2, 1, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, int, s, 64, 1, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, uint, u, 8, 8, 3, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, uint, u, 16, 4, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, uint, u, 32, 2, 3, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(, uint, u, 64, 1, 3, expected_cumulative_sat, CMT);

  TEST_VQSHL_N(q, int, s, 8, 16, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, int, s, 16, 8, 1, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, int, s, 32, 4, 1, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, int, s, 64, 2, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, uint, u, 8, 16, 3, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, uint, u, 16, 8, 2, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, uint, u, 32, 4, 3, expected_cumulative_sat, CMT);
  TEST_VQSHL_N(q, uint, u, 64, 2, 3, expected_cumulative_sat, CMT);

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


  /* Fill input vector with max value, to check saturation on limits.  */
  VDUP(vector, , int, s, 8, 8, 0x7F);
  VDUP(vector, , int, s, 16, 4, 0x7FFF);
  VDUP(vector, , int, s, 32, 2, 0x7FFFFFFF);
  VDUP(vector, , int, s, 64, 1, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, , uint, u, 8, 8, 0xFF);
  VDUP(vector, , uint, u, 16, 4, 0xFFFF);
  VDUP(vector, , uint, u, 32, 2, 0xFFFFFFFF);
  VDUP(vector, , uint, u, 64, 1, 0xFFFFFFFFFFFFFFFFULL);
  VDUP(vector, q, int, s, 8, 16, 0x7F);
  VDUP(vector, q, int, s, 16, 8, 0x7FFF);
  VDUP(vector, q, int, s, 32, 4, 0x7FFFFFFF);
  VDUP(vector, q, int, s, 64, 2, 0x7FFFFFFFFFFFFFFFLL);
  VDUP(vector, q, uint, u, 8, 16, 0xFF);
  VDUP(vector, q, uint, u, 16, 8, 0xFFFF);
  VDUP(vector, q, uint, u, 32, 4, 0xFFFFFFFF);
  VDUP(vector, q, uint, u, 64, 2, 0xFFFFFFFFFFFFFFFFULL);

#undef CMT
#define CMT " (with max input)"
  TEST_VQSHL_N(, int, s, 8, 8, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, int, s, 16, 4, 1, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, int, s, 32, 2, 1, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, int, s, 64, 1, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, uint, u, 8, 8, 3, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, uint, u, 16, 4, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, uint, u, 32, 2, 3, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(, uint, u, 64, 1, 3, expected_cumulative_sat_max, CMT);

  TEST_VQSHL_N(q, int, s, 8, 16, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, int, s, 16, 8, 1, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, int, s, 32, 4, 1, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, int, s, 64, 2, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, uint, u, 8, 16, 3, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, uint, u, 16, 8, 2, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, uint, u, 32, 4, 3, expected_cumulative_sat_max, CMT);
  TEST_VQSHL_N(q, uint, u, 64, 2, 3, expected_cumulative_sat_max, CMT);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected_max, CMT);
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected_max, CMT);
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected_max, CMT);
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected_max, CMT);
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected_max, CMT);
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected_max, CMT);
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected_max, CMT);
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected_max, CMT);
  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected_max, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected_max, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected_max, CMT);
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected_max, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_max, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_max, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_max, CMT);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected_max, CMT);
}

int main (void)
{
  exec_vqshl_n ();
  return 0;
}

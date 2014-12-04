#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results for vcvt.  */
VECT_VAR_DECL(expected_s,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_u,hfloat,32,2) [] = { 0x4f800000, 0x4f800000 };
VECT_VAR_DECL(expected_s,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					   0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_u,hfloat,32,4) [] = { 0x4f800000, 0x4f800000,
					   0x4f800000, 0x4f800000 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0x5 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x0, 0x5 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x0, 0x0, 0xf, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x0, 0xf, 0x0 };

/* Expected results for vcvt_n.  */
VECT_VAR_DECL(expected_vcvt_n_s,hfloat,32,2) [] = { 0xc0800000, 0xc0700000 };
VECT_VAR_DECL(expected_vcvt_n_u,hfloat,32,2) [] = { 0x4c000000, 0x4c000000 };
VECT_VAR_DECL(expected_vcvt_n_s,hfloat,32,4) [] = { 0xb2800000, 0xb2700000,
						  0xb2600000, 0xb2500000 };
VECT_VAR_DECL(expected_vcvt_n_u,hfloat,32,4) [] = { 0x49800000, 0x49800000,
						  0x49800000, 0x49800000 };
VECT_VAR_DECL(expected_vcvt_n,int,32,2) [] = { 0xff0b3333, 0x54cccd };
VECT_VAR_DECL(expected_vcvt_n,uint,32,2) [] = { 0x0, 0x15 };
VECT_VAR_DECL(expected_vcvt_n,int,32,4) [] = { 0x0, 0x0, 0x1e3d7, 0xfffe1c29 };
VECT_VAR_DECL(expected_vcvt_n,uint,32,4) [] = { 0x0, 0x0, 0x1e, 0x0 };

/* Expected results for vcvt with rounding.  */
VECT_VAR_DECL(expected_rounding,int,32,2) [] = { 0xa, 0xa };
VECT_VAR_DECL(expected_rounding,uint,32,2) [] = { 0xa, 0xa };
VECT_VAR_DECL(expected_rounding,int,32,4) [] = { 0x7d, 0x7d, 0x7d, 0x7d };
VECT_VAR_DECL(expected_rounding,uint,32,4) [] = { 0x7d, 0x7d, 0x7d, 0x7d };

/* Expected results for vcvt_n with rounding.  */
VECT_VAR_DECL(expected_vcvt_n_rounding,int,32,2) [] = { 0xa66666, 0xa66666 };
VECT_VAR_DECL(expected_vcvt_n_rounding,uint,32,2) [] = { 0xa66666, 0xa66666 };
VECT_VAR_DECL(expected_vcvt_n_rounding,int,32,4) [] = { 0xfbccc, 0xfbccc,
					       0xfbccc, 0xfbccc };
VECT_VAR_DECL(expected_vcvt_n_rounding,uint,32,4) [] = { 0xfbccc, 0xfbccc,
						0xfbccc, 0xfbccc };

/* Expected results for vcvt_n with saturation.  */
VECT_VAR_DECL(expected_vcvt_n_saturation,int,32,2) [] = { 0x7fffffff,
							  0x7fffffff };
VECT_VAR_DECL(expected_vcvt_n_saturation,int,32,4) [] = { 0x7fffffff,
							  0x7fffffff,
					       0x7fffffff, 0x7fffffff };

#define TEST_MSG "VCVT/VCVTQ"
void exec_vcvt (void)
{
  int i;

  /* Basic test: y=vcvt(x), then store the result.  */
#define TEST_VCVT(Q, T1, T2, W, N, TS1, TS2, EXP)		\
  VECT_VAR(vector_res, T1, W, N) =				\
    vcvt##Q##_##T2##W##_##TS2##W(VECT_VAR(vector, TS1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		    VECT_VAR(vector_res, T1, W, N));		\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXP, TEST_MSG2);

#define TEST_VCVT_FP(Q, T1, T2, W, N, TS1, TS2, EXP)		\
  VECT_VAR(vector_res, T1, W, N) =				\
    vcvt##Q##_##T2##W##_##TS2##W(VECT_VAR(vector, TS1, W, N));	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),			\
		    VECT_VAR(vector_res, T1, W, N));		\
  CHECK_FP(TEST_MSG, T1, W, N, PRIx##W, EXP, TEST_MSG2);

#define TEST_VCVT_N(Q, T1, T2, W, N, TS1, TS2, V, EXP)			\
  VECT_VAR(vector_res, T1, W, N) =					\
    vcvt##Q##_n_##T2##W##_##TS2##W(VECT_VAR(vector, TS1, W, N), V);	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK(TEST_MSG, T1, W, N, PRIx##W, EXP, TEST_MSG2);

#define TEST_VCVT_N_FP(Q, T1, T2, W, N, TS1, TS2, V, EXP)		\
  VECT_VAR(vector_res, T1, W, N) =					\
    vcvt##Q##_n_##T2##W##_##TS2##W(VECT_VAR(vector, TS1, W, N), V);	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N),				\
		    VECT_VAR(vector_res, T1, W, N));			\
  CHECK_FP(TEST_MSG, T1, W, N, PRIx##W, EXP, TEST_MSG2);

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Make sure some elements have a fractional part, to exercise
     integer conversions.  */
  VSET_LANE(vector, , float, f, 32, 2, 0, -15.3f);
  VSET_LANE(vector, , float, f, 32, 2, 1, 5.3f);
  VSET_LANE(vector, q, float, f, 32, 4, 2, -15.3f);
  VSET_LANE(vector, q, float, f, 32, 4, 3, 5.3f);

  /* The same result buffers are used multiple times, so we check them
     before overwriting them.  */
#define TEST_MSG2 ""

  /* vcvt_f32_xx.  */
  TEST_VCVT_FP(, float, f, 32, 2, int, s, expected_s);
  TEST_VCVT_FP(, float, f, 32, 2, uint, u, expected_u);

  /* vcvtq_f32_xx.  */
  TEST_VCVT_FP(q, float, f, 32, 4, int, s, expected_s);
  TEST_VCVT_FP(q, float, f, 32, 4, uint, u, expected_u);

  /* vcvt_xx_f32.  */
  TEST_VCVT(, int, s, 32, 2, float, f, expected);
  TEST_VCVT(, uint, u, 32, 2, float, f, expected);

  VSET_LANE(vector, q, float, f, 32, 4, 0, 0.0f);
  VSET_LANE(vector, q, float, f, 32, 4, 1, -0.0f);
  VSET_LANE(vector, q, float, f, 32, 4, 2, 15.12f);
  VSET_LANE(vector, q, float, f, 32, 4, 3, -15.12f);

  /* vcvtq_xx_f32.  */
  TEST_VCVT(q, int, s, 32, 4, float, f, expected);
  TEST_VCVT(q, uint, u, 32, 4, float, f, expected);

  /* The same result buffers are used multiple times, so we check them
     before overwriting them.  */
#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"

  /* vcvt_n_f32_xx.  */
  TEST_VCVT_N_FP(, float, f, 32, 2, int, s, 2, expected_vcvt_n_s);
  TEST_VCVT_N_FP(, float, f, 32, 2, uint, u, 7, expected_vcvt_n_u);

  /* vcvtq_n_f32_xx.  */
  TEST_VCVT_N_FP(q, float, f, 32, 4, int, s, 30, expected_vcvt_n_s);
  TEST_VCVT_N_FP(q, float, f, 32, 4, uint, u, 12, expected_vcvt_n_u);

  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 20, expected_vcvt_n);
  TEST_VCVT_N(, uint, u, 32, 2, float, f, 2, expected_vcvt_n);

  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 13, expected_vcvt_n);
  TEST_VCVT_N(q, uint, u, 32, 4, float, f, 1, expected_vcvt_n);

  /* Check rounding.  */
#undef TEST_MSG
#define TEST_MSG "VCVT/VCVTQ"
#undef TEST_MSG2
#define TEST_MSG2 "(check rounding)"
  VDUP(vector, , float, f, 32, 2, 10.4f);
  VDUP(vector, q, float, f, 32, 4, 125.9f);
  /* vcvt_xx_f32.  */
  TEST_VCVT(, int, s, 32, 2, float, f, expected_rounding);
  TEST_VCVT(, uint, u, 32, 2, float, f, expected_rounding);
  /* vcvtq_xx_f32.  */
  TEST_VCVT(q, int, s, 32, 4, float, f, expected_rounding);
  TEST_VCVT(q, uint, u, 32, 4, float, f, expected_rounding);

#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"
  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 20, expected_vcvt_n_rounding);
  TEST_VCVT_N(, uint, u, 32, 2, float, f, 20, expected_vcvt_n_rounding);
  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 13, expected_vcvt_n_rounding);
  TEST_VCVT_N(q, uint, u, 32, 4, float, f, 13, expected_vcvt_n_rounding);

#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"
#undef TEST_MSG2
#define TEST_MSG2 "(check saturation)"
  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 31, expected_vcvt_n_saturation);
  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 31, expected_vcvt_n_saturation);
}

int main (void)
{
  exec_vcvt ();
  return 0;
}

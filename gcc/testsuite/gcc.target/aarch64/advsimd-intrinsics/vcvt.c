#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"
#include <math.h>

/* Expected results for vcvt.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_s, hfloat, 16, 4) [] =
{ 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected_u, hfloat, 16, 4) [] =
{ 0x7c00, 0x7c00, 0x7c00, 0x7c00, };
VECT_VAR_DECL(expected_s, hfloat, 16, 8) [] =
{ 0xcc00, 0xcb80, 0xcb00, 0xca80,
  0xca00, 0xc980, 0xc900, 0xc880 };
VECT_VAR_DECL(expected_u, hfloat, 16, 8) [] =
{ 0x7c00, 0x7c00, 0x7c00, 0x7c00,
  0x7c00, 0x7c00, 0x7c00, 0x7c00, };
#endif
VECT_VAR_DECL(expected_s,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected_u,hfloat,32,2) [] = { 0x4f800000, 0x4f800000 };
VECT_VAR_DECL(expected_s,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					     0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected_u,hfloat,32,4) [] = { 0x4f800000, 0x4f800000,
					     0x4f800000, 0x4f800000 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected, int, 16, 4) [] = { 0xfff1, 0x5, 0xfff1, 0x5 };
VECT_VAR_DECL(expected, uint, 16, 4) [] = { 0x0, 0x5, 0x0, 0x5 };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x0, 0x0, 0xf, 0xfff1,
					   0x0, 0x0, 0xf, 0xfff1 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x0, 0x0, 0xf, 0x0,
					    0x0, 0x0, 0xf, 0x0 };
#endif
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0x5 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0x0, 0x5 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x0, 0x0, 0xf, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x0, 0x0, 0xf, 0x0 };

/* Expected results for vcvt_n.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_vcvt_n_s, hfloat, 16, 4) [] = { 0xc400, 0xc380,
						       0xc300, 0xc280 };
VECT_VAR_DECL(expected_vcvt_n_u, hfloat, 16, 4) [] = { 0x6000, 0x6000,
						       0x6000, 0x6000 };
VECT_VAR_DECL(expected_vcvt_n_s, hfloat, 16, 8) [] = { 0xb000, 0xaf80,
						       0xaf00, 0xae80,
						       0xae00, 0xad80,
						       0xad00, 0xac80 };
VECT_VAR_DECL(expected_vcvt_n_u, hfloat, 16, 8) [] = { 0x4c00, 0x4c00,
						       0x4c00, 0x4c00,
						       0x4c00, 0x4c00,
						       0x4c00, 0x4c00 };
#endif
VECT_VAR_DECL(expected_vcvt_n_s,hfloat,32,2) [] = { 0xc0800000, 0xc0700000 };
VECT_VAR_DECL(expected_vcvt_n_u,hfloat,32,2) [] = { 0x4c000000, 0x4c000000 };
VECT_VAR_DECL(expected_vcvt_n_s,hfloat,32,4) [] = { 0xb2800000, 0xb2700000,
						  0xb2600000, 0xb2500000 };
VECT_VAR_DECL(expected_vcvt_n_u,hfloat,32,4) [] = { 0x49800000, 0x49800000,
						  0x49800000, 0x49800000 };
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_vcvt_n, int, 16, 4) [] = { 0xffc3, 0x15,
						  0xffc3, 0x15 };
VECT_VAR_DECL(expected_vcvt_n, uint, 16, 4) [] = { 0x0, 0x2a6, 0x0, 0x2a6 };
VECT_VAR_DECL(expected_vcvt_n, int, 16, 8) [] = { 0x0, 0x0, 0x78f, 0xf871,
						  0x0, 0x0, 0x78f, 0xf871 };
VECT_VAR_DECL(expected_vcvt_n, uint, 16, 8) [] = { 0x0, 0x0, 0xf1e0, 0x0,
						   0x0, 0x0, 0xf1e0, 0x0 };
#endif
VECT_VAR_DECL(expected_vcvt_n,int,32,2) [] = { 0xff0b3333, 0x54cccd };
VECT_VAR_DECL(expected_vcvt_n,uint,32,2) [] = { 0x0, 0x15 };
VECT_VAR_DECL(expected_vcvt_n,int,32,4) [] = { 0x0, 0x0, 0x1e3d7, 0xfffe1c29 };
VECT_VAR_DECL(expected_vcvt_n,uint,32,4) [] = { 0x0, 0x0, 0x1e, 0x0 };

/* Expected results for vcvt with rounding.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_rounding, int, 16, 4) [] = { 0xa, 0xa, 0xa, 0xa };
VECT_VAR_DECL(expected_rounding, uint, 16, 4) [] = { 0xa, 0xa, 0xa, 0xa };
VECT_VAR_DECL(expected_rounding, int, 16, 8) [] = { 0x7d, 0x7d, 0x7d, 0x7d,
						    0x7d, 0x7d, 0x7d, 0x7d };
VECT_VAR_DECL(expected_rounding, uint, 16, 8) [] = { 0x7d, 0x7d, 0x7d, 0x7d,
						     0x7d, 0x7d, 0x7d, 0x7d };
#endif
VECT_VAR_DECL(expected_rounding,int,32,2) [] = { 0xa, 0xa };
VECT_VAR_DECL(expected_rounding,uint,32,2) [] = { 0xa, 0xa };
VECT_VAR_DECL(expected_rounding,int,32,4) [] = { 0x7d, 0x7d, 0x7d, 0x7d };
VECT_VAR_DECL(expected_rounding,uint,32,4) [] = { 0x7d, 0x7d, 0x7d, 0x7d };

/* Expected results for vcvt_n with rounding.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_vcvt_n_rounding, int, 16, 4) [] =
{ 0x533, 0x533, 0x533, 0x533 };
VECT_VAR_DECL(expected_vcvt_n_rounding, uint, 16, 4) [] =
{ 0x533, 0x533, 0x533, 0x533 };
VECT_VAR_DECL(expected_vcvt_n_rounding, int, 16, 8) [] =
{ 0x7fff, 0x7fff, 0x7fff, 0x7fff,
  0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected_vcvt_n_rounding, uint, 16, 8) [] =
{ 0xffff, 0xffff, 0xffff, 0xffff,
  0xffff, 0xffff, 0xffff, 0xffff };
#endif
VECT_VAR_DECL(expected_vcvt_n_rounding,int,32,2) [] = { 0xa66666, 0xa66666 };
VECT_VAR_DECL(expected_vcvt_n_rounding,uint,32,2) [] = { 0xa66666, 0xa66666 };
VECT_VAR_DECL(expected_vcvt_n_rounding,int,32,4) [] = { 0xfbccc, 0xfbccc,
					       0xfbccc, 0xfbccc };
VECT_VAR_DECL(expected_vcvt_n_rounding,uint,32,4) [] = { 0xfbccc, 0xfbccc,
						0xfbccc, 0xfbccc };

/* Expected results for vcvt_n with saturation.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
VECT_VAR_DECL(expected_vcvt_n_saturation, int, 16, 4) [] =
{ 0x533, 0x533, 0x533, 0x533 };
VECT_VAR_DECL(expected_vcvt_n_saturation, int, 16, 8) [] =
{ 0x7fff, 0x7fff, 0x7fff, 0x7fff,
  0x7fff, 0x7fff, 0x7fff, 0x7fff };
#endif
VECT_VAR_DECL(expected_vcvt_n_saturation,int,32,2) [] =
{ 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected_vcvt_n_saturation,int,32,4) [] =
{ 0x7fffffff, 0x7fffffff, 0x7fffffff, 0x7fffffff };

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
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Make sure some elements have a fractional part, to exercise
     integer conversions.  */
#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VSET_LANE(vector, , float, f, 16, 4, 0, -15.3f);
  VSET_LANE(vector, , float, f, 16, 4, 1, 5.3f);
  VSET_LANE(vector, , float, f, 16, 4, 2, -15.3f);
  VSET_LANE(vector, , float, f, 16, 4, 3, 5.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 4, -15.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 5, 5.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 6, -15.3f);
  VSET_LANE(vector, q, float, f, 16, 8, 7, 5.3f);
#endif

  VSET_LANE(vector, , float, f, 32, 2, 0, -15.3f);
  VSET_LANE(vector, , float, f, 32, 2, 1, 5.3f);
  VSET_LANE(vector, q, float, f, 32, 4, 2, -15.3f);
  VSET_LANE(vector, q, float, f, 32, 4, 3, 5.3f);

  /* The same result buffers are used multiple times, so we check them
     before overwriting them.  */
#define TEST_MSG2 ""

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_f16_xx.  */
  TEST_VCVT_FP(, float, f, 16, 4, int, s, expected_s);
  TEST_VCVT_FP(, float, f, 16, 4, uint, u, expected_u);
#endif
  /* vcvt_f32_xx.  */
  TEST_VCVT_FP(, float, f, 32, 2, int, s, expected_s);
  TEST_VCVT_FP(, float, f, 32, 2, uint, u, expected_u);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_f16_xx.  */
  TEST_VCVT_FP(q, float, f, 16, 8, int, s, expected_s);
  TEST_VCVT_FP(q, float, f, 16, 8, uint, u, expected_u);
#endif
  /* vcvtq_f32_xx.  */
  TEST_VCVT_FP(q, float, f, 32, 4, int, s, expected_s);
  TEST_VCVT_FP(q, float, f, 32, 4, uint, u, expected_u);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_xx_f16.  */
  TEST_VCVT(, int, s, 16, 4, float, f, expected);
  TEST_VCVT(, uint, u, 16, 4, float, f, expected);
#endif
  /* vcvt_xx_f32.  */
  TEST_VCVT(, int, s, 32, 2, float, f, expected);
  TEST_VCVT(, uint, u, 32, 2, float, f, expected);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VSET_LANE(vector, q, float, f, 16, 8, 0, 0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 1, -0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 2, 15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 3, -15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 4, 0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 5, -0.0f);
  VSET_LANE(vector, q, float, f, 16, 8, 6, 15.12f);
  VSET_LANE(vector, q, float, f, 16, 8, 7, -15.12f);
#endif

  VSET_LANE(vector, q, float, f, 32, 4, 0, 0.0f);
  VSET_LANE(vector, q, float, f, 32, 4, 1, -0.0f);
  VSET_LANE(vector, q, float, f, 32, 4, 2, 15.12f);
  VSET_LANE(vector, q, float, f, 32, 4, 3, -15.12f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_xx_f16.  */
  TEST_VCVT(q, int, s, 16, 8, float, f, expected);
  TEST_VCVT(q, uint, u, 16, 8, float, f, expected);
#endif

  /* vcvtq_xx_f32.  */
  TEST_VCVT(q, int, s, 32, 4, float, f, expected);
  TEST_VCVT(q, uint, u, 32, 4, float, f, expected);

  /* The same result buffers are used multiple times, so we check them
     before overwriting them.  */
#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_n_f16_xx.  */
  TEST_VCVT_N_FP(, float, f, 16, 4, int, s, 2, expected_vcvt_n_s);
  TEST_VCVT_N_FP(, float, f, 16, 4, uint, u, 7, expected_vcvt_n_u);
#endif
  /* vcvt_n_f32_xx.  */
  TEST_VCVT_N_FP(, float, f, 32, 2, int, s, 2, expected_vcvt_n_s);
  TEST_VCVT_N_FP(, float, f, 32, 2, uint, u, 7, expected_vcvt_n_u);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_n_f16_xx.  */
  TEST_VCVT_N_FP(q, float, f, 16, 8, int, s, 7, expected_vcvt_n_s);
  TEST_VCVT_N_FP(q, float, f, 16, 8, uint, u, 12, expected_vcvt_n_u);
#endif
  /* vcvtq_n_f32_xx.  */
  TEST_VCVT_N_FP(q, float, f, 32, 4, int, s, 30, expected_vcvt_n_s);
  TEST_VCVT_N_FP(q, float, f, 32, 4, uint, u, 12, expected_vcvt_n_u);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_n_xx_f16.  */
  TEST_VCVT_N(, int, s, 16, 4, float, f, 2, expected_vcvt_n);
  TEST_VCVT_N(, uint, u, 16, 4, float, f, 7, expected_vcvt_n);
#endif
  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 20, expected_vcvt_n);
  TEST_VCVT_N(, uint, u, 32, 2, float, f, 2, expected_vcvt_n);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_n_xx_f16.  */
  TEST_VCVT_N(q, int, s, 16, 8, float, f, 7, expected_vcvt_n);
  TEST_VCVT_N(q, uint, u, 16, 8, float, f, 12, expected_vcvt_n);
#endif
  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 13, expected_vcvt_n);
  TEST_VCVT_N(q, uint, u, 32, 4, float, f, 1, expected_vcvt_n);

  /* Check rounding.  */
#undef TEST_MSG
#define TEST_MSG "VCVT/VCVTQ"
#undef TEST_MSG2
#define TEST_MSG2 "(check rounding)"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  VDUP(vector, , float, f, 16, 4, 10.4f);
  VDUP(vector, q, float, f, 16, 8, 125.9f);
#endif
  VDUP(vector, , float, f, 32, 2, 10.4f);
  VDUP(vector, q, float, f, 32, 4, 125.9f);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_xx_f16.  */
  TEST_VCVT(, int, s, 16, 4, float, f, expected_rounding);
  TEST_VCVT(, uint, u, 16, 4, float, f, expected_rounding);
#endif
  /* vcvt_xx_f32.  */
  TEST_VCVT(, int, s, 32, 2, float, f, expected_rounding);
  TEST_VCVT(, uint, u, 32, 2, float, f, expected_rounding);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_xx_f16.  */
  TEST_VCVT(q, int, s, 16, 8, float, f, expected_rounding);
  TEST_VCVT(q, uint, u, 16, 8, float, f, expected_rounding);
#endif
  /* vcvtq_xx_f32.  */
  TEST_VCVT(q, int, s, 32, 4, float, f, expected_rounding);
  TEST_VCVT(q, uint, u, 32, 4, float, f, expected_rounding);

#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_n_xx_f16.  */
  TEST_VCVT_N(, int, s, 16, 4, float, f, 7, expected_vcvt_n_rounding);
  TEST_VCVT_N(, uint, u, 16, 4, float, f, 7, expected_vcvt_n_rounding);
#endif
  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 20, expected_vcvt_n_rounding);
  TEST_VCVT_N(, uint, u, 32, 2, float, f, 20, expected_vcvt_n_rounding);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_n_xx_f16.  */
  TEST_VCVT_N(q, int, s, 16, 8, float, f, 13, expected_vcvt_n_rounding);
  TEST_VCVT_N(q, uint, u, 16, 8, float, f, 13, expected_vcvt_n_rounding);
#endif
  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 13, expected_vcvt_n_rounding);
  TEST_VCVT_N(q, uint, u, 32, 4, float, f, 13, expected_vcvt_n_rounding);

#undef TEST_MSG
#define TEST_MSG "VCVT_N/VCVTQ_N"
#undef TEST_MSG2
#define TEST_MSG2 "(check saturation)"

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvt_n_xx_f16.  */
  TEST_VCVT_N(, int, s, 16, 4, float, f, 7, expected_vcvt_n_saturation);
#endif
  /* vcvt_n_xx_f32.  */
  TEST_VCVT_N(, int, s, 32, 2, float, f, 31, expected_vcvt_n_saturation);

#if defined (__ARM_FEATURE_FP16_VECTOR_ARITHMETIC)
  /* vcvtq_n_xx_f16.  */
  TEST_VCVT_N(q, int, s, 16, 8, float, f, 13, expected_vcvt_n_saturation);
#endif
  /* vcvtq_n_xx_f32.  */
  TEST_VCVT_N(q, int, s, 32, 4, float, f, 31, expected_vcvt_n_saturation);
}

int main (void)
{
  exec_vcvt ();
  return 0;
}

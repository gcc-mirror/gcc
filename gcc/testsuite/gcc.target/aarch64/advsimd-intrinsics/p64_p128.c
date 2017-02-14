/* This file contains tests for all the *p64 intrinsics, except for
   vreinterpret which have their own testcase.  */

/* { dg-require-effective-target arm_crypto_ok { target { arm*-*-* } } } */
/* { dg-add-options arm_crypto } */
/* { dg-additional-options "-march=armv8-a+crypto" { target { aarch64*-*-* } } }*/

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results: vbsl.  */
VECT_VAR_DECL(vbsl_expected,poly,64,1) [] = { 0xfffffff1 };
VECT_VAR_DECL(vbsl_expected,poly,64,2) [] = { 0xfffffff1,
					      0xfffffff1 };

/* Expected results: vceq.  */
VECT_VAR_DECL(vceq_expected,uint,64,1) [] = { 0x0 };

/* Expected results: vcombine.  */
VECT_VAR_DECL(vcombine_expected,poly,64,2) [] = { 0xfffffffffffffff0, 0x88 };

/* Expected results: vcreate.  */
VECT_VAR_DECL(vcreate_expected,poly,64,1) [] = { 0x123456789abcdef0 };

/* Expected results: vdup_lane.  */
VECT_VAR_DECL(vdup_lane_expected,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vdup_lane_expected,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff0 };

/* Expected results: vdup_n.  */
VECT_VAR_DECL(vdup_n_expected0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vdup_n_expected0,poly,64,2) [] = { 0xfffffffffffffff0,
						 0xfffffffffffffff0 };
VECT_VAR_DECL(vdup_n_expected1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vdup_n_expected1,poly,64,2) [] = { 0xfffffffffffffff1,
						 0xfffffffffffffff1 };
VECT_VAR_DECL(vdup_n_expected2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vdup_n_expected2,poly,64,2) [] = { 0xfffffffffffffff2,
						 0xfffffffffffffff2 };

/* Expected results: vext.  */
VECT_VAR_DECL(vext_expected,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vext_expected,poly,64,2) [] = { 0xfffffffffffffff1, 0x88 };

/* Expected results: vget_low.  */
VECT_VAR_DECL(vget_low_expected,poly,64,1) [] = { 0xfffffffffffffff0 };

/* Expected results: vget_high.  */
VECT_VAR_DECL(vget_high_expected,poly,64,1) [] = { 0xfffffffffffffff1 };

/* Expected results: vld1.  */
VECT_VAR_DECL(vld1_expected,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld1_expected,poly,64,2) [] = { 0xfffffffffffffff0,
					      0xfffffffffffffff1 };

/* Expected results: vld1_dup.  */
VECT_VAR_DECL(vld1_dup_expected0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld1_dup_expected0,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff0 };
VECT_VAR_DECL(vld1_dup_expected1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld1_dup_expected1,poly,64,2) [] = { 0xfffffffffffffff1,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(vld1_dup_expected2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vld1_dup_expected2,poly,64,2) [] = { 0xfffffffffffffff2,
						   0xfffffffffffffff2 };

/* Expected results: vld1_lane.  */
VECT_VAR_DECL(vld1_lane_expected,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld1_lane_expected,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xaaaaaaaaaaaaaaaa };

/* Expected results: vldX.  */
VECT_VAR_DECL(vld2_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld2_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld3_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld3_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld3_expected_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vld4_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld4_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld4_expected_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vld4_expected_3,poly,64,1) [] = { 0xfffffffffffffff3 };

/* Expected results: vldX_dup.  */
VECT_VAR_DECL(vld2_dup_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld2_dup_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld3_dup_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld3_dup_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld3_dup_expected_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vld4_dup_expected_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vld4_dup_expected_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vld4_dup_expected_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vld4_dup_expected_3,poly,64,1) [] = { 0xfffffffffffffff3 };

/* Expected results: vsli.  */
VECT_VAR_DECL(vsli_expected,poly,64,1) [] = { 0x10 };
VECT_VAR_DECL(vsli_expected,poly,64,2) [] = { 0x7ffffffffffff0,
					      0x7ffffffffffff1 };
VECT_VAR_DECL(vsli_expected_max_shift,poly,64,1) [] = { 0x7ffffffffffffff0 };
VECT_VAR_DECL(vsli_expected_max_shift,poly,64,2) [] = { 0xfffffffffffffff0,
							0xfffffffffffffff1 };

/* Expected results: vsri.  */
VECT_VAR_DECL(vsri_expected,poly,64,1) [] = { 0xe000000000000000 };
VECT_VAR_DECL(vsri_expected,poly,64,2) [] = { 0xfffffffffffff800,
					      0xfffffffffffff800 };
VECT_VAR_DECL(vsri_expected_max_shift,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vsri_expected_max_shift,poly,64,2) [] = { 0xfffffffffffffff0,
							0xfffffffffffffff1 };

/* Expected results: vst1_lane.  */
VECT_VAR_DECL(vst1_lane_expected,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vst1_lane_expected,poly,64,2) [] = { 0xfffffffffffffff0,
						   0x3333333333333333 };

/* Expected results: vget_lane.  */
VECT_VAR_DECL(vget_lane_expected,poly,64,1) = 0xfffffffffffffff0;
VECT_VAR_DECL(vget_lane_expected,poly,64,2) = 0xfffffffffffffff0;

/* Expected results: vset_lane.  */
VECT_VAR_DECL(vset_lane_expected,poly,64,1) [] = { 0x88 };
VECT_VAR_DECL(vset_lane_expected,poly,64,2) [] = { 0xfffffffffffffff0, 0x11 };

/* Expected results: vtst.  */
VECT_VAR_DECL(vtst_expected,uint,64,1) [] = { 0x0 };

#ifdef __aarch64__
/* Expected results: vmov_n.  */
VECT_VAR_DECL(vmov_n_expected0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(vmov_n_expected0,poly,64,2) [] = { 0xfffffffffffffff0,
						 0xfffffffffffffff0 };
VECT_VAR_DECL(vmov_n_expected1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(vmov_n_expected1,poly,64,2) [] = { 0xfffffffffffffff1,
						 0xfffffffffffffff1 };
VECT_VAR_DECL(vmov_n_expected2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(vmov_n_expected2,poly,64,2) [] = { 0xfffffffffffffff2,
						 0xfffffffffffffff2 };

/* Expected results: vldX_lane.  */
VECT_VAR_DECL(expected_vld_st2_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld_st2_0,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st2_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st2_1,poly,64,2) [] = { 0xaaaaaaaaaaaaaaaa,
						   0xaaaaaaaaaaaaaaaa };
VECT_VAR_DECL(expected_vld_st3_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld_st3_0,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st3_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st3_1,poly,64,2) [] = { 0xfffffffffffffff2,
						   0xaaaaaaaaaaaaaaaa };
VECT_VAR_DECL(expected_vld_st3_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld_st3_2,poly,64,2) [] = { 0xaaaaaaaaaaaaaaaa,
						   0xaaaaaaaaaaaaaaaa };
VECT_VAR_DECL(expected_vld_st4_0,poly,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected_vld_st4_0,poly,64,2) [] = { 0xfffffffffffffff0,
						   0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st4_1,poly,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected_vld_st4_1,poly,64,2) [] = { 0xfffffffffffffff2,
						   0xfffffffffffffff3 };
VECT_VAR_DECL(expected_vld_st4_2,poly,64,1) [] = { 0xfffffffffffffff2 };
VECT_VAR_DECL(expected_vld_st4_2,poly,64,2) [] = { 0xaaaaaaaaaaaaaaaa,
						   0xaaaaaaaaaaaaaaaa };
VECT_VAR_DECL(expected_vld_st4_3,poly,64,1) [] = { 0xfffffffffffffff3 };
VECT_VAR_DECL(expected_vld_st4_3,poly,64,2) [] = { 0xaaaaaaaaaaaaaaaa,
						   0xaaaaaaaaaaaaaaaa };

/* Expected results: vtst.  */
VECT_VAR_DECL(vtst_expected,uint,64,2) [] = { 0x0, 0xffffffffffffffff };
#endif

int main (void)
{
  int i;

  /* vbsl_p64 tests.  */
#define TEST_MSG "VBSL/VBSLQ"

#define TEST_VBSL(T3, Q, T1, T2, W, N)					\
  VECT_VAR(vbsl_vector_res, T1, W, N) =					\
    vbsl##Q##_##T2##W(VECT_VAR(vbsl_vector_first, T3, W, N),		\
		      VECT_VAR(vbsl_vector, T1, W, N),			\
		      VECT_VAR(vbsl_vector2, T1, W, N));		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vbsl_vector_res, T1, W, N))

  DECL_VARIABLE(vbsl_vector, poly, 64, 1);
  DECL_VARIABLE(vbsl_vector, poly, 64, 2);
  DECL_VARIABLE(vbsl_vector2, poly, 64, 1);
  DECL_VARIABLE(vbsl_vector2, poly, 64, 2);
  DECL_VARIABLE(vbsl_vector_res, poly, 64, 1);
  DECL_VARIABLE(vbsl_vector_res, poly, 64, 2);

  DECL_VARIABLE(vbsl_vector_first, uint, 64, 1);
  DECL_VARIABLE(vbsl_vector_first, uint, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vbsl_vector, buffer, , poly, p, 64, 1);
  VLOAD(vbsl_vector, buffer, q, poly, p, 64, 2);

  VDUP(vbsl_vector2, , poly, p, 64, 1, 0xFFFFFFF3);
  VDUP(vbsl_vector2, q, poly, p, 64, 2, 0xFFFFFFF3);

  VDUP(vbsl_vector_first, , uint, u, 64, 1, 0xFFFFFFF2);
  VDUP(vbsl_vector_first, q, uint, u, 64, 2, 0xFFFFFFF2);

  TEST_VBSL(uint, , poly, p, 64, 1);
  TEST_VBSL(uint, q, poly, p, 64, 2);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vbsl_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vbsl_expected, "");

  /* vceq_p64 tests. */
#undef TEST_MSG
#define TEST_MSG "VCEQ"

#define TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)				\
  VECT_VAR(vceq_vector_res, T3, W, N) =					\
    INSN##Q##_##T2##W(VECT_VAR(vceq_vector, T1, W, N),			\
		      VECT_VAR(vceq_vector2, T1, W, N));		\
  vst1##Q##_u##W(VECT_VAR(result, T3, W, N), VECT_VAR(vceq_vector_res, T3, W, N))

#define TEST_VCOMP(INSN, Q, T1, T2, T3, W, N)				\
  TEST_VCOMP1(INSN, Q, T1, T2, T3, W, N)

  DECL_VARIABLE(vceq_vector, poly, 64, 1);
  DECL_VARIABLE(vceq_vector2, poly, 64, 1);
  DECL_VARIABLE(vceq_vector_res, uint, 64, 1);

  CLEAN(result, uint, 64, 1);

  VLOAD(vceq_vector, buffer, , poly, p, 64, 1);

  VDUP(vceq_vector2, , poly, p, 64, 1, 0x88);

  TEST_VCOMP(vceq, , poly, p, uint, 64, 1);

  CHECK(TEST_MSG, uint, 64, 1, PRIx64, vceq_expected, "");

  /* vcombine_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VCOMBINE"

#define TEST_VCOMBINE(T1, T2, W, N, N2)					\
  VECT_VAR(vcombine_vector128, T1, W, N2) =				\
    vcombine_##T2##W(VECT_VAR(vcombine_vector64_a, T1, W, N),		\
		     VECT_VAR(vcombine_vector64_b, T1, W, N));		\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N2), VECT_VAR(vcombine_vector128, T1, W, N2))

  DECL_VARIABLE(vcombine_vector64_a, poly, 64, 1);
  DECL_VARIABLE(vcombine_vector64_b, poly, 64, 1);
  DECL_VARIABLE(vcombine_vector128, poly, 64, 2);

  CLEAN(result, poly, 64, 2);

  VLOAD(vcombine_vector64_a, buffer, , poly, p, 64, 1);

  VDUP(vcombine_vector64_b, , poly, p, 64, 1, 0x88);

  TEST_VCOMBINE(poly, p, 64, 1, 2);

  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vcombine_expected, "");

  /* vcreate_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VCREATE"

#define TEST_VCREATE(T1, T2, W, N)					\
  VECT_VAR(vcreate_vector_res, T1, W, N) =				\
    vcreate_##T2##W(VECT_VAR(vcreate_val, T1, W, N));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vcreate_vector_res, T1, W, N))

#define DECL_VAL(VAR, T1, W, N)			\
  uint64_t VECT_VAR(VAR, T1, W, N)

  DECL_VAL(vcreate_val, poly, 64, 1);
  DECL_VARIABLE(vcreate_vector_res, poly, 64, 1);

  CLEAN(result, poly, 64, 2);

  VECT_VAR(vcreate_val, poly, 64, 1) = 0x123456789abcdef0ULL;

  TEST_VCREATE(poly, p, 64, 1);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vcreate_expected, "");

  /* vdup_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VDUP_LANE/VDUP_LANEQ"

#define TEST_VDUP_LANE(Q, T1, T2, W, N, N2, L)				\
  VECT_VAR(vdup_lane_vector_res, T1, W, N) =				\
    vdup##Q##_lane_##T2##W(VECT_VAR(vdup_lane_vector, T1, W, N2), L);	\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vdup_lane_vector_res, T1, W, N))

  DECL_VARIABLE(vdup_lane_vector, poly, 64, 1);
  DECL_VARIABLE(vdup_lane_vector, poly, 64, 2);
  DECL_VARIABLE(vdup_lane_vector_res, poly, 64, 1);
  DECL_VARIABLE(vdup_lane_vector_res, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vdup_lane_vector, buffer, , poly, p, 64, 1);

  TEST_VDUP_LANE(, poly, p, 64, 1, 1, 0);
  TEST_VDUP_LANE(q, poly, p, 64, 2, 1, 0);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vdup_lane_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vdup_lane_expected, "");

  /* vdup_n_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VDUP/VDUPQ"

#define TEST_VDUP(Q, T1, T2, W, N)					\
  VECT_VAR(vdup_n_vector, T1, W, N) =					\
    vdup##Q##_n_##T2##W(VECT_VAR(buffer_dup, T1, W, N)[i]);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vdup_n_vector, T1, W, N))

  DECL_VARIABLE(vdup_n_vector, poly, 64, 1);
  DECL_VARIABLE(vdup_n_vector, poly, 64, 2);

  /* Try to read different places from the input buffer.  */
  for (i=0; i< 3; i++) {
    CLEAN(result, poly, 64, 1);
    CLEAN(result, poly, 64, 2);

    TEST_VDUP(, poly, p, 64, 1);
    TEST_VDUP(q, poly, p, 64, 2);

    switch (i) {
    case 0:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vdup_n_expected0, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vdup_n_expected0, "");
      break;
    case 1:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vdup_n_expected1, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vdup_n_expected1, "");
      break;
    case 2:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vdup_n_expected2, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vdup_n_expected2, "");
      break;
    default:
      abort();
    }
  }

  /* vexit_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VEXT/VEXTQ"

#define TEST_VEXT(Q, T1, T2, W, N, V)					\
  VECT_VAR(vext_vector_res, T1, W, N) =					\
    vext##Q##_##T2##W(VECT_VAR(vext_vector1, T1, W, N),			\
		      VECT_VAR(vext_vector2, T1, W, N),			\
		      V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vext_vector_res, T1, W, N))

  DECL_VARIABLE(vext_vector1, poly, 64, 1);
  DECL_VARIABLE(vext_vector1, poly, 64, 2);
  DECL_VARIABLE(vext_vector2, poly, 64, 1);
  DECL_VARIABLE(vext_vector2, poly, 64, 2);
  DECL_VARIABLE(vext_vector_res, poly, 64, 1);
  DECL_VARIABLE(vext_vector_res, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vext_vector1, buffer, , poly, p, 64, 1);
  VLOAD(vext_vector1, buffer, q, poly, p, 64, 2);

  VDUP(vext_vector2, , poly, p, 64, 1, 0x88);
  VDUP(vext_vector2, q, poly, p, 64, 2, 0x88);

  TEST_VEXT(, poly, p, 64, 1, 0);
  TEST_VEXT(q, poly, p, 64, 2, 1);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vext_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vext_expected, "");

  /* vget_low_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VGET_LOW"

#define TEST_VGET_LOW(T1, T2, W, N, N2)					\
  VECT_VAR(vget_low_vector64, T1, W, N) =				\
    vget_low_##T2##W(VECT_VAR(vget_low_vector128, T1, W, N2));		\
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vget_low_vector64, T1, W, N))

  DECL_VARIABLE(vget_low_vector64, poly, 64, 1);
  DECL_VARIABLE(vget_low_vector128, poly, 64, 2);

  CLEAN(result, poly, 64, 1);

  VLOAD(vget_low_vector128, buffer, q, poly, p, 64, 2);

  TEST_VGET_LOW(poly, p, 64, 1, 2);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vget_low_expected, "");

  /* vget_high_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VGET_HIGH"

#define TEST_VGET_HIGH(T1, T2, W, N, N2)					\
  VECT_VAR(vget_high_vector64, T1, W, N) =				\
    vget_high_##T2##W(VECT_VAR(vget_high_vector128, T1, W, N2));		\
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vget_high_vector64, T1, W, N))

  DECL_VARIABLE(vget_high_vector64, poly, 64, 1);
  DECL_VARIABLE(vget_high_vector128, poly, 64, 2);

  CLEAN(result, poly, 64, 1);

  VLOAD(vget_high_vector128, buffer, q, poly, p, 64, 2);

  TEST_VGET_HIGH(poly, p, 64, 1, 2);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vget_high_expected, "");

  /* vld1_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VLD1/VLD1Q"

#define TEST_VLD1(VAR, BUF, Q, T1, T2, W, N)				\
  VECT_VAR(VAR, T1, W, N) = vld1##Q##_##T2##W(VECT_VAR(BUF, T1, W, N)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(VAR, T1, W, N))

  DECL_VARIABLE(vld1_vector, poly, 64, 1);
  DECL_VARIABLE(vld1_vector, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vld1_vector, buffer, , poly, p, 64, 1);
  VLOAD(vld1_vector, buffer, q, poly, p, 64, 2);

  TEST_VLD1(vld1_vector, buffer, , poly, p, 64, 1);
  TEST_VLD1(vld1_vector, buffer, q, poly, p, 64, 2);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld1_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vld1_expected, "");

  /* vld1_dup_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VLD1_DUP/VLD1_DUPQ"

#define TEST_VLD1_DUP(VAR, BUF, Q, T1, T2, W, N)			\
  VECT_VAR(VAR, T1, W, N) =						\
    vld1##Q##_dup_##T2##W(&VECT_VAR(BUF, T1, W, N)[i]);			\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(VAR, T1, W, N))

  DECL_VARIABLE(vld1_dup_vector, poly, 64, 1);
  DECL_VARIABLE(vld1_dup_vector, poly, 64, 2);

  /* Try to read different places from the input buffer.  */
  for (i=0; i<3; i++) {
    CLEAN(result, poly, 64, 1);
    CLEAN(result, poly, 64, 2);

    TEST_VLD1_DUP(vld1_dup_vector, buffer_dup, , poly, p, 64, 1);
    TEST_VLD1_DUP(vld1_dup_vector, buffer_dup, q, poly, p, 64, 2);

    switch (i) {
    case 0:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld1_dup_expected0, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vld1_dup_expected0, "");
      break;
    case 1:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld1_dup_expected1, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vld1_dup_expected1, "");
      break;
    case 2:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld1_dup_expected2, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vld1_dup_expected2, "");
      break;
    default:
      abort();
    }
  }

  /* vld1_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VLD1_LANE/VLD1_LANEQ"

#define TEST_VLD1_LANE(Q, T1, T2, W, N, L)				\
  memset (VECT_VAR(vld1_lane_buffer_src, T1, W, N), 0xAA, W/8*N);	\
  VECT_VAR(vld1_lane_vector_src, T1, W, N) =				\
    vld1##Q##_##T2##W(VECT_VAR(vld1_lane_buffer_src, T1, W, N));	\
  VECT_VAR(vld1_lane_vector, T1, W, N) =				\
    vld1##Q##_lane_##T2##W(VECT_VAR(buffer, T1, W, N),			\
			   VECT_VAR(vld1_lane_vector_src, T1, W, N), L); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vld1_lane_vector, T1, W, N))

  DECL_VARIABLE(vld1_lane_vector, poly, 64, 1);
  DECL_VARIABLE(vld1_lane_vector, poly, 64, 2);
  DECL_VARIABLE(vld1_lane_vector_src, poly, 64, 1);
  DECL_VARIABLE(vld1_lane_vector_src, poly, 64, 2);

  ARRAY(vld1_lane_buffer_src, poly, 64, 1);
  ARRAY(vld1_lane_buffer_src, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  TEST_VLD1_LANE(, poly, p, 64, 1, 0);
  TEST_VLD1_LANE(q, poly, p, 64, 2, 0);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld1_lane_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vld1_lane_expected, "");

  /* vldX_p64 tests.  */
#define DECL_VLDX(T1, W, N, X)						\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vldX_vector, T1, W, N, X); \
  VECT_VAR_DECL(vldX_result_bis_##X, T1, W, N)[X * N]

#define TEST_VLDX(Q, T1, T2, W, N, X)					\
  VECT_ARRAY_VAR(vldX_vector, T1, W, N, X) =				\
    /* Use dedicated init buffer, of size X */				\
    vld##X##Q##_##T2##W(VECT_ARRAY_VAR(buffer_vld##X, T1, W, N, X));	\
  vst##X##Q##_##T2##W(VECT_VAR(vldX_result_bis_##X, T1, W, N),		\
		      VECT_ARRAY_VAR(vldX_vector, T1, W, N, X));	\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(vldX_result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[Y].  */
#define TEST_EXTRA_CHUNK(T1, W, N, X,Y)				\
  memcpy(VECT_VAR(result, T1, W, N),				\
	 &(VECT_VAR(vldX_result_bis_##X, T1, W, N)[Y*N]),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  DECL_VLDX(poly, 64, 1, 2);
  DECL_VLDX(poly, 64, 1, 3);
  DECL_VLDX(poly, 64, 1, 4);

  VECT_ARRAY_INIT2(buffer_vld2, poly, 64, 1);
  PAD(buffer_vld2_pad, poly, 64, 1);
  VECT_ARRAY_INIT3(buffer_vld3, poly, 64, 1);
  PAD(buffer_vld3_pad, poly, 64, 1);
  VECT_ARRAY_INIT4(buffer_vld4, poly, 64, 1);
  PAD(buffer_vld4_pad, poly, 64, 1);

#undef TEST_MSG
#define TEST_MSG "VLD2/VLD2Q"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX(, poly, p, 64, 1, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld2_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 2, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld2_expected_1, "chunk 1");

#undef TEST_MSG
#define TEST_MSG "VLD3/VLD3Q"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX(, poly, p, 64, 1, 3);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 3, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_expected_1, "chunk 1");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 3, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_expected_2, "chunk 2");

#undef TEST_MSG
#define TEST_MSG "VLD4/VLD4Q"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX(, poly, p, 64, 1, 4);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 4, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_expected_1, "chunk 1");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 4, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_expected_2, "chunk 2");
  CLEAN(result, poly, 64, 1);
  TEST_EXTRA_CHUNK(poly, 64, 1, 4, 3);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_expected_3, "chunk 3");

  /* vldX_dup_p64 tests.  */
#define DECL_VLDX_DUP(T1, W, N, X)					\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vldX_dup_vector, T1, W, N, X); \
  VECT_VAR_DECL(vldX_dup_result_bis_##X, T1, W, N)[X * N]

#define TEST_VLDX_DUP(Q, T1, T2, W, N, X)				\
  VECT_ARRAY_VAR(vldX_dup_vector, T1, W, N, X) =			\
    vld##X##Q##_dup_##T2##W(&VECT_VAR(buffer_dup, T1, W, N)[0]);	\
    									\
  vst##X##Q##_##T2##W(VECT_VAR(vldX_dup_result_bis_##X, T1, W, N),	\
		      VECT_ARRAY_VAR(vldX_dup_vector, T1, W, N, X));	\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(vldX_dup_result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Overwrite "result" with the contents of "result_bis"[Y].  */
#define TEST_VLDX_DUP_EXTRA_CHUNK(T1, W, N, X,Y)		\
  memcpy(VECT_VAR(result, T1, W, N),				\
	 &(VECT_VAR(vldX_dup_result_bis_##X, T1, W, N)[Y*N]),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  DECL_VLDX_DUP(poly, 64, 1, 2);
  DECL_VLDX_DUP(poly, 64, 1, 3);
  DECL_VLDX_DUP(poly, 64, 1, 4);


#undef TEST_MSG
#define TEST_MSG "VLD2_DUP/VLD2Q_DUP"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP(, poly, p, 64, 1, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld2_dup_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 2, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld2_dup_expected_1, "chunk 1");

#undef TEST_MSG
#define TEST_MSG "VLD3_DUP/VLD3Q_DUP"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP(, poly, p, 64, 1, 3);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_dup_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 3, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_dup_expected_1, "chunk 1");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 3, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld3_dup_expected_2, "chunk 2");

#undef TEST_MSG
#define TEST_MSG "VLD4_DUP/VLD4Q_DUP"
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP(, poly, p, 64, 1, 4);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_dup_expected_0, "chunk 0");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 4, 1);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_dup_expected_1, "chunk 1");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 4, 2);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_dup_expected_2, "chunk 2");
  CLEAN(result, poly, 64, 1);
  TEST_VLDX_DUP_EXTRA_CHUNK(poly, 64, 1, 4, 3);
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vld4_dup_expected_3, "chunk 3");

  /* vsli_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VSLI"

#define TEST_VSXI1(INSN, Q, T1, T2, W, N, V)				\
  VECT_VAR(vsXi_vector_res, T1, W, N) =					\
    INSN##Q##_n_##T2##W(VECT_VAR(vsXi_vector, T1, W, N),		\
		      VECT_VAR(vsXi_vector2, T1, W, N),			\
		      V);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vsXi_vector_res, T1, W, N))

#define TEST_VSXI(INSN, Q, T1, T2, W, N, V)	\
  TEST_VSXI1(INSN, Q, T1, T2, W, N, V)

  DECL_VARIABLE(vsXi_vector, poly, 64, 1);
  DECL_VARIABLE(vsXi_vector, poly, 64, 2);
  DECL_VARIABLE(vsXi_vector2, poly, 64, 1);
  DECL_VARIABLE(vsXi_vector2, poly, 64, 2);
  DECL_VARIABLE(vsXi_vector_res, poly, 64, 1);
  DECL_VARIABLE(vsXi_vector_res, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vsXi_vector, buffer, , poly, p, 64, 1);
  VLOAD(vsXi_vector, buffer, q, poly, p, 64, 2);

  VDUP(vsXi_vector2, , poly, p, 64, 1, 2);
  VDUP(vsXi_vector2, q, poly, p, 64, 2, 3);

  TEST_VSXI(vsli, , poly, p, 64, 1, 3);
  TEST_VSXI(vsli, q, poly, p, 64, 2, 53);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vsli_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vsli_expected, "");

  /* Test cases with maximum shift amount.  */
  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  TEST_VSXI(vsli, , poly, p, 64, 1, 63);
  TEST_VSXI(vsli, q, poly, p, 64, 2, 63);

#define COMMENT "(max shift amount)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vsli_expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vsli_expected_max_shift, COMMENT);

  /* vsri_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VSRI"

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  VLOAD(vsXi_vector, buffer, , poly, p, 64, 1);
  VLOAD(vsXi_vector, buffer, q, poly, p, 64, 2);

  VDUP(vsXi_vector2, , poly, p, 64, 1, 2);
  VDUP(vsXi_vector2, q, poly, p, 64, 2, 3);

  TEST_VSXI(vsri, , poly, p, 64, 1, 3);
  TEST_VSXI(vsri, q, poly, p, 64, 2, 53);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vsri_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vsri_expected, "");

  /* Test cases with maximum shift amount.  */
  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  TEST_VSXI(vsri, , poly, p, 64, 1, 64);
  TEST_VSXI(vsri, q, poly, p, 64, 2, 64);

#define COMMENT "(max shift amount)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vsri_expected_max_shift, COMMENT);
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vsri_expected_max_shift, COMMENT);

  /* vst1_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VST1_LANE/VST1_LANEQ"

#define TEST_VST1_LANE(Q, T1, T2, W, N, L)				\
  VECT_VAR(vst1_lane_vector, T1, W, N) =				\
    vld1##Q##_##T2##W(VECT_VAR(buffer, T1, W, N));			\
  vst1##Q##_lane_##T2##W(VECT_VAR(result, T1, W, N),			\
			 VECT_VAR(vst1_lane_vector, T1, W, N), L);

  DECL_VARIABLE(vst1_lane_vector, poly, 64, 1);
  DECL_VARIABLE(vst1_lane_vector, poly, 64, 2);

  CLEAN(result, poly, 64, 1);
  CLEAN(result, poly, 64, 2);

  TEST_VST1_LANE(, poly, p, 64, 1, 0);
  TEST_VST1_LANE(q, poly, p, 64, 2, 0);

  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vst1_lane_expected, "");
  CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vst1_lane_expected, "");

  /* vget_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VGET_LANE/VGETQ_LANE"

#define TEST_VGET_LANE(Q, T1, T2, W, N, L)				   \
  VECT_VAR(vget_lane_vector, T1, W, N) = vget##Q##_lane_##T2##W(VECT_VAR(vget_lane_vector1, T1, W, N), L); \
  if (VECT_VAR(vget_lane_vector, T1, W, N) != VECT_VAR(vget_lane_expected, T1, W, N)) {		\
    fprintf(stderr,							   \
	    "ERROR in %s (%s line %d in result '%s') at type %s "	   \
	    "got 0x%" PRIx##W " != 0x%" PRIx##W "\n",			   \
	    TEST_MSG, __FILE__, __LINE__,				   \
	    STR(VECT_VAR(vget_lane_expected, T1, W, N)),		   \
	    STR(VECT_NAME(T1, W, N)),					   \
	    VECT_VAR(vget_lane_vector, T1, W, N),			   \
	    VECT_VAR(vget_lane_expected, T1, W, N));			   \
    abort ();								   \
  }

  /* Initialize input values.  */
  DECL_VARIABLE(vget_lane_vector1, poly, 64, 1);
  DECL_VARIABLE(vget_lane_vector1, poly, 64, 2);

  VLOAD(vget_lane_vector1, buffer,  , poly, p, 64, 1);
  VLOAD(vget_lane_vector1, buffer, q, poly, p, 64, 2);

  VECT_VAR_DECL(vget_lane_vector, poly, 64, 1);
  VECT_VAR_DECL(vget_lane_vector, poly, 64, 2);

  TEST_VGET_LANE( , poly, p, 64, 1, 0);
  TEST_VGET_LANE(q, poly, p, 64, 2, 0);


  /* vset_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VSET_LANE/VSETQ_LANE"

#define TEST_VSET_LANE(Q, T1, T2, W, N, V, L)				\
  VECT_VAR(vset_lane_vector, T1, W, N) =						\
    vset##Q##_lane_##T2##W(V,						\
			   VECT_VAR(vset_lane_vector, T1, W, N),			\
			   L);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vset_lane_vector, T1, W, N))

  /* Initialize input values.  */
  DECL_VARIABLE(vset_lane_vector, poly, 64, 1);
  DECL_VARIABLE(vset_lane_vector, poly, 64, 2);

  CLEAN(result, uint, 64, 1);
  CLEAN(result, uint, 64, 2);

  VLOAD(vset_lane_vector, buffer, , poly, p, 64, 1);
  VLOAD(vset_lane_vector, buffer, q, poly, p, 64, 2);

  /* Choose value and lane arbitrarily.  */
  TEST_VSET_LANE(, poly, p, 64, 1, 0x88, 0);
  TEST_VSET_LANE(q, poly, p, 64, 2, 0x11, 1);

  CHECK(TEST_MSG, poly, 64, 1, PRIx64, vset_lane_expected, "");
  CHECK(TEST_MSG, poly, 64, 2, PRIx64, vset_lane_expected, "");


  /* vtst_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VTST"
  
#define TEST_VTST1(INSN, Q, T1, T2, W, N)			\
  VECT_VAR(vtst_vector_res, uint, W, N) =			\
    INSN##Q##_##T2##W(VECT_VAR(vtst_vector, T1, W, N),		\
		      VECT_VAR(vtst_vector2, T1, W, N));	\
    vst1##Q##_u##W(VECT_VAR(result, uint, W, N),		\
		   VECT_VAR(vtst_vector_res, uint, W, N))

#define TEST_VTST(INSN, Q, T1, T2, W, N)	\
  TEST_VTST1(INSN, Q, T1, T2, W, N)		\

  /* Initialize input values.  */
  DECL_VARIABLE(vtst_vector, poly, 64, 1);
  DECL_VARIABLE(vtst_vector2, poly, 64, 1);
  DECL_VARIABLE(vtst_vector_res, uint, 64, 1);

  CLEAN(result, uint, 64, 1);

  VLOAD(vtst_vector, buffer,  , poly, p, 64, 1);
  VDUP(vtst_vector2, , poly, p, 64, 1, 5);

  TEST_VTST(vtst, , poly, p, 64, 1);

  CHECK(TEST_MSG, uint, 64, 1, PRIx64, vtst_expected, "");

  /* vtstq_p64 is supported by aarch64 only.  */
#ifdef __aarch64__
  DECL_VARIABLE(vtst_vector, poly, 64, 2);
  DECL_VARIABLE(vtst_vector2, poly, 64, 2);
  DECL_VARIABLE(vtst_vector_res, uint, 64, 2);
  CLEAN(result, uint, 64, 2);
  VLOAD(vtst_vector, buffer, q, poly, p, 64, 2);
  VDUP(vtst_vector2, q, poly, p, 64, 2, 5);
  TEST_VTST(vtst, q, poly, p, 64, 2);
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, vtst_expected, "");

  /* vmov_n_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VMOV/VMOVQ"

#define TEST_VMOV(Q, T1, T2, W, N)					\
  VECT_VAR(vmov_n_vector, T1, W, N) =					\
    vmov##Q##_n_##T2##W(VECT_VAR(buffer_dup, T1, W, N)[i]);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vmov_n_vector, T1, W, N))

  DECL_VARIABLE(vmov_n_vector, poly, 64, 1);
  DECL_VARIABLE(vmov_n_vector, poly, 64, 2);

  /* Try to read different places from the input buffer.  */
  for (i=0; i< 3; i++) {
    CLEAN(result, poly, 64, 1);
    CLEAN(result, poly, 64, 2);

    TEST_VMOV(, poly, p, 64, 1);
    TEST_VMOV(q, poly, p, 64, 2);

    switch (i) {
    case 0:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vmov_n_expected0, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vmov_n_expected0, "");
      break;
    case 1:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vmov_n_expected1, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vmov_n_expected1, "");
      break;
    case 2:
      CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, vmov_n_expected2, "");
      CHECK_POLY(TEST_MSG, poly, 64, 2, PRIx64, vmov_n_expected2, "");
      break;
    default:
      abort();
    }
  }

  /* vldx_lane_p64 tests.  */
#undef TEST_MSG
#define TEST_MSG "VLDX_LANE/VLDXQ_LANE"

VECT_VAR_DECL_INIT(buffer_vld2_lane, poly, 64, 2);
VECT_VAR_DECL_INIT(buffer_vld3_lane, poly, 64, 3);
VECT_VAR_DECL_INIT(buffer_vld4_lane, poly, 64, 4);

  /* In this case, input variables are arrays of vectors.  */
#define DECL_VLD_STX_LANE(T1, W, N, X)					\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector, T1, W, N, X);	\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector_src, T1, W, N, X);	\
  VECT_VAR_DECL(result_bis_##X, T1, W, N)[X * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behavior. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
  /* We also use another extra input buffer (buffer_src), which we
     fill with 0xAA, and which it used to load a vector from which we
     read a given lane.  */

#define TEST_VLDX_LANE(Q, T1, T2, W, N, X, L)				\
  memset (VECT_VAR(buffer_src, T1, W, N), 0xAA,				\
	  sizeof(VECT_VAR(buffer_src, T1, W, N)));			\
									\
  VECT_ARRAY_VAR(vector_src, T1, W, N, X) =				\
    vld##X##Q##_##T2##W(VECT_VAR(buffer_src, T1, W, N));		\
									\
  VECT_ARRAY_VAR(vector, T1, W, N, X) =					\
    /* Use dedicated init buffer, of size.  X */			\
    vld##X##Q##_lane_##T2##W(VECT_VAR(buffer_vld##X##_lane, T1, W, X),	\
			     VECT_ARRAY_VAR(vector_src, T1, W, N, X),	\
			     L);					\
  vst##X##Q##_##T2##W(VECT_VAR(result_bis_##X, T1, W, N),		\
		      VECT_ARRAY_VAR(vector, T1, W, N, X));		\
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)))

  /* Overwrite "result" with the contents of "result_bis"[Y].  */
#undef TEST_EXTRA_CHUNK
#define TEST_EXTRA_CHUNK(T1, W, N, X, Y)		\
  memcpy(VECT_VAR(result, T1, W, N),			\
	 &(VECT_VAR(result_bis_##X, T1, W, N)[Y*N]),	\
	 sizeof(VECT_VAR(result, T1, W, N)));

  /* Add some padding to try to catch out of bound accesses.  */
#define ARRAY1(V, T, W, N) VECT_VAR_DECL(V,T,W,N)[1]={42}
#define DUMMY_ARRAY(V, T, W, N, L) \
  VECT_VAR_DECL(V,T,W,N)[N*L]={0}; \
  ARRAY1(V##_pad,T,W,N)

#define DECL_ALL_VLD_STX_LANE(X)     \
  DECL_VLD_STX_LANE(poly, 64, 1, X); \
  DECL_VLD_STX_LANE(poly, 64, 2, X);

#define TEST_ALL_VLDX_LANE(X)		  \
  TEST_VLDX_LANE(, poly, p, 64, 1, X, 0); \
  TEST_VLDX_LANE(q, poly, p, 64, 2, X, 0);

#define TEST_ALL_EXTRA_CHUNKS(X,Y)	     \
  TEST_EXTRA_CHUNK(poly, 64, 1, X, Y) \
  TEST_EXTRA_CHUNK(poly, 64, 2, X, Y)

#define CHECK_RESULTS_VLD_STX_LANE(test_name,EXPECTED,comment)		\
  CHECK_POLY(test_name, poly, 64, 1, PRIx64, EXPECTED, comment);	\
  CHECK_POLY(test_name, poly, 64, 2, PRIx64, EXPECTED, comment);

  /* Declare the temporary buffers / variables.  */
  DECL_ALL_VLD_STX_LANE(2);
  DECL_ALL_VLD_STX_LANE(3);
  DECL_ALL_VLD_STX_LANE(4);

  DUMMY_ARRAY(buffer_src, poly, 64, 1, 4);
  DUMMY_ARRAY(buffer_src, poly, 64, 2, 4);

  /* Check vld2_lane/vld2q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD2_LANE/VLD2Q_LANE"
  TEST_ALL_VLDX_LANE(2);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st2_0, " chunk 0");

  TEST_ALL_EXTRA_CHUNKS(2, 1);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st2_1, " chunk 1");

  /* Check vld3_lane/vld3q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD3_LANE/VLD3Q_LANE"
  TEST_ALL_VLDX_LANE(3);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st3_0, " chunk 0");

  TEST_ALL_EXTRA_CHUNKS(3, 1);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st3_1, " chunk 1");

  TEST_ALL_EXTRA_CHUNKS(3, 2);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st3_2, " chunk 2");

  /* Check vld4_lane/vld4q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VLD4_LANE/VLD4Q_LANE"
  TEST_ALL_VLDX_LANE(4);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st4_0, " chunk 0");

  TEST_ALL_EXTRA_CHUNKS(4, 1);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st4_1, " chunk 1");

  TEST_ALL_EXTRA_CHUNKS(4, 2);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st4_2, " chunk 2");

  TEST_ALL_EXTRA_CHUNKS(4, 3);
  CHECK_RESULTS_VLD_STX_LANE (TEST_MSG, expected_vld_st4_3, " chunk 3");

  /* In this case, input variables are arrays of vectors.  */
#define DECL_VSTX_LANE(T1, W, N, X)					\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector, T1, W, N, X);	\
  VECT_ARRAY_TYPE(T1, W, N, X) VECT_ARRAY_VAR(vector_src, T1, W, N, X);	\
  VECT_VAR_DECL(result_bis_##X, T1, W, N)[X * N]

  /* We need to use a temporary result buffer (result_bis), because
     the one used for other tests is not large enough. A subset of the
     result data is moved from result_bis to result, and it is this
     subset which is used to check the actual behavior. The next
     macro enables to move another chunk of data from result_bis to
     result.  */
  /* We also use another extra input buffer (buffer_src), which we
     fill with 0xAA, and which it used to load a vector from which we
     read a given lane.  */
#define TEST_VSTX_LANE(Q, T1, T2, W, N, X, L)				 \
  memset (VECT_VAR(buffer_src, T1, W, N), 0xAA,				 \
	  sizeof(VECT_VAR(buffer_src, T1, W, N)));			 \
  memset (VECT_VAR(result_bis_##X, T1, W, N), 0,			 \
	  sizeof(VECT_VAR(result_bis_##X, T1, W, N)));			 \
									 \
  VECT_ARRAY_VAR(vector_src, T1, W, N, X) =				 \
    vld##X##Q##_##T2##W(VECT_VAR(buffer_src, T1, W, N));		 \
									 \
  VECT_ARRAY_VAR(vector, T1, W, N, X) =					 \
    /* Use dedicated init buffer, of size X.  */			 \
    vld##X##Q##_lane_##T2##W(VECT_VAR(buffer_vld##X##_lane, T1, W, X),	 \
			     VECT_ARRAY_VAR(vector_src, T1, W, N, X),	 \
			     L);					 \
  vst##X##Q##_lane_##T2##W(VECT_VAR(result_bis_##X, T1, W, N),		 \
			   VECT_ARRAY_VAR(vector, T1, W, N, X),		 \
			   L);						 \
  memcpy(VECT_VAR(result, T1, W, N), VECT_VAR(result_bis_##X, T1, W, N), \
	 sizeof(VECT_VAR(result, T1, W, N)));

#define TEST_ALL_VSTX_LANE(X)		  \
  TEST_VSTX_LANE(, poly, p, 64, 1, X, 0); \
  TEST_VSTX_LANE(q, poly, p, 64, 2, X, 0);

  /* Check vst2_lane/vst2q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST2_LANE/VST2Q_LANE"
  TEST_ALL_VSTX_LANE(2);

#define CMT " (chunk 0)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st2_0, CMT);

  TEST_ALL_EXTRA_CHUNKS(2, 1);
#undef CMT
#define CMT " chunk 1"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st2_1, CMT);

  /* Check vst3_lane/vst3q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST3_LANE/VST3Q_LANE"
  TEST_ALL_VSTX_LANE(3);

#undef CMT
#define CMT " (chunk 0)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st3_0, CMT);

  TEST_ALL_EXTRA_CHUNKS(3, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st3_1, CMT);

  TEST_ALL_EXTRA_CHUNKS(3, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st3_2, CMT);

  /* Check vst4_lane/vst4q_lane.  */
  clean_results ();
#undef TEST_MSG
#define TEST_MSG "VST4_LANE/VST4Q_LANE"
  TEST_ALL_VSTX_LANE(4);

#undef CMT
#define CMT " (chunk 0)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st4_0, CMT);

  TEST_ALL_EXTRA_CHUNKS(4, 1);

#undef CMT
#define CMT " (chunk 1)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st4_1, CMT);

  TEST_ALL_EXTRA_CHUNKS(4, 2);

#undef CMT
#define CMT " (chunk 2)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st4_2, CMT);

  TEST_ALL_EXTRA_CHUNKS(4, 3);

#undef CMT
#define CMT " (chunk 3)"
  CHECK_POLY(TEST_MSG, poly, 64, 1, PRIx64, expected_vld_st4_3, CMT);

#endif /* __aarch64__.  */

  return 0;
}

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
				       0xaa, 0xaa, 0xf0, 0xaa };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xfff0 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xaaaaaaaa, 0xfffffff0 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					0xaa, 0xaa, 0xaa, 0xf0 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xfff0 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xaaaaaaaa, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					0xaa, 0xaa, 0xaa, 0xf0 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xfff0 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					   0xaa, 0xf0, 0xaa, 0xaa };
#endif
VECT_VAR_DECL(expected,hfloat,16,4) [] = { 0xaaaa, 0xaaaa, 0xcc00, 0xaaaa };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xaaaaaaaa, 0xc1800000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					0xaa, 0xaa, 0xaa, 0xaa,
					0xaa, 0xaa, 0xaa, 0xaa,
					0xaa, 0xaa, 0xaa, 0xf0 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa,
					0xaaaa, 0xfff0, 0xaaaa, 0xaaaa };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xaaaaaaaa, 0xaaaaaaaa,
					0xfffffff0, 0xaaaaaaaa };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xaaaaaaaaaaaaaaaa,
					0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					 0xaa, 0xaa, 0xaa, 0xaa,
					 0xaa, 0xaa, 0xaa, 0xaa,
					 0xf0, 0xaa, 0xaa, 0xaa };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa,
					 0xaaaa, 0xaaaa, 0xfff0, 0xaaaa };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xaaaaaaaa, 0xaaaaaaaa,
					 0xfffffff0, 0xaaaaaaaa };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0xaaaaaaaaaaaaaaaa };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					 0xaa, 0xaa, 0xaa, 0xaa,
					 0xaa, 0xaa, 0xaa, 0xaa,
					 0xf0, 0xaa, 0xaa, 0xaa };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa,
					 0xaaaa, 0xaaaa, 0xfff0, 0xaaaa };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xaa, 0xaa, 0xaa, 0xaa,
					    0xaa, 0xaa, 0xaa, 0xaa,
					    0xaa, 0xaa, 0xaa, 0xf0,
					    0xaa, 0xaa, 0xaa, 0xaa };
#endif
VECT_VAR_DECL(expected,hfloat,16,8) [] = { 0xaaaa, 0xaaaa, 0xaaaa, 0xaaaa,
					   0xaaaa, 0xcc00, 0xaaaa, 0xaaaa };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xaaaaaaaa, 0xaaaaaaaa,
					   0xc1800000, 0xaaaaaaaa };

#define TEST_MSG "VLD1_LANE/VLD1_LANEQ"
void exec_vld1_lane (void)
{
  /* Fill vector_src with 0xAA, then load 1 lane.  */
#define TEST_VLD1_LANE(Q, T1, T2, W, N, L)				\
  memset (VECT_VAR(buffer_src, T1, W, N), 0xAA, W/8*N);			\
  VECT_VAR(vector_src, T1, W, N) =					\
    vld1##Q##_##T2##W(VECT_VAR(buffer_src, T1, W, N));			\
  VECT_VAR(vector, T1, W, N) =						\
    vld1##Q##_lane_##T2##W(VECT_VAR(buffer, T1, W, N),			\
			   VECT_VAR(vector_src, T1, W, N), L);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);
  DECL_VARIABLE_ALL_VARIANTS(vector_src);

  ARRAY(buffer_src, int, 8, 8);
  ARRAY(buffer_src, int, 16, 4);
  ARRAY(buffer_src, int, 32, 2);
  ARRAY(buffer_src, int, 64, 1);
  ARRAY(buffer_src, uint, 8, 8);
  ARRAY(buffer_src, uint, 16, 4);
  ARRAY(buffer_src, uint, 32, 2);
  ARRAY(buffer_src, uint, 64, 1);
  ARRAY(buffer_src, poly, 8, 8);
  ARRAY(buffer_src, poly, 16, 4);
  MFLOAT8_ONLY(ARRAY(buffer_src, mfloat, 8, 8));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  ARRAY(buffer_src, float, 16, 4);
#endif
  ARRAY(buffer_src, float, 32, 2);

  ARRAY(buffer_src, int, 8, 16);
  ARRAY(buffer_src, int, 16, 8);
  ARRAY(buffer_src, int, 32, 4);
  ARRAY(buffer_src, int, 64, 2);
  ARRAY(buffer_src, uint, 8, 16);
  ARRAY(buffer_src, uint, 16, 8);
  ARRAY(buffer_src, uint, 32, 4);
  ARRAY(buffer_src, uint, 64, 2);
  ARRAY(buffer_src, poly, 8, 16);
  ARRAY(buffer_src, poly, 16, 8);
  MFLOAT8_ONLY(ARRAY(buffer_src, mfloat, 8, 16));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  ARRAY(buffer_src, float, 16, 8);
#endif
  ARRAY(buffer_src, float, 32, 4);

  clean_results ();

  /* Choose lane arbitrarily.  */
  TEST_VLD1_LANE(, int, s, 8, 8, 6);
  TEST_VLD1_LANE(, int, s, 16, 4, 3);
  TEST_VLD1_LANE(, int, s, 32, 2, 1);
  TEST_VLD1_LANE(, int, s, 64, 1, 0);
  TEST_VLD1_LANE(, uint, u, 8, 8, 7);
  TEST_VLD1_LANE(, uint, u, 16, 4, 3);
  TEST_VLD1_LANE(, uint, u, 32, 2, 1);
  TEST_VLD1_LANE(, uint, u, 64, 1, 0);
  TEST_VLD1_LANE(, poly, p, 8, 8, 7);
  TEST_VLD1_LANE(, poly, p, 16, 4, 3);
  MFLOAT8_ONLY(TEST_VLD1_LANE(, mfloat, mf, 8, 8, 5));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VLD1_LANE(, float, f, 16, 4, 2);
#endif
  TEST_VLD1_LANE(, float, f, 32, 2, 1);

  TEST_VLD1_LANE(q, int, s, 8, 16, 15);
  TEST_VLD1_LANE(q, int, s, 16, 8, 5);
  TEST_VLD1_LANE(q, int, s, 32, 4, 2);
  TEST_VLD1_LANE(q, int, s, 64, 2, 1);
  TEST_VLD1_LANE(q, uint, u, 8, 16, 12);
  TEST_VLD1_LANE(q, uint, u, 16, 8, 6);
  TEST_VLD1_LANE(q, uint, u, 32, 4, 2);
  TEST_VLD1_LANE(q, uint, u, 64, 2, 0);
  TEST_VLD1_LANE(q, poly, p, 8, 16, 12);
  TEST_VLD1_LANE(q, poly, p, 16, 8, 6);
  MFLOAT8_ONLY(TEST_VLD1_LANE(q, mfloat, mf, 8, 16, 11));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VLD1_LANE(q, float, f, 16, 8, 5);
#endif
  TEST_VLD1_LANE(q, float, f, 32, 4, 2);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  exec_vld1_lane ();
  return 0;
}

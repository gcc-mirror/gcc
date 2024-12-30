#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf1, 0xf1, 0xf1, 0xf1,
				       0xf1, 0xf1, 0xf1, 0xf1 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf7, 0xf7, 0xf7, 0xf7,
					0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff3, 0xfff3, 0xfff3, 0xfff3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf7, 0xf7, 0xf7, 0xf7,
					0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff3, 0xfff3, 0xfff3, 0xfff3 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xf6, 0xf6, 0xf6, 0xf6,
					   0xf6, 0xf6, 0xf6, 0xf6 };
#endif
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1700000, 0xc1700000 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 4) [] = { 0xca80, 0xca80,
					       0xca80, 0xca80 };
#endif
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf2, 0xf2, 0xf2, 0xf2,
					0xf2, 0xf2, 0xf2, 0xf2,
					0xf2, 0xf2, 0xf2, 0xf2,
					0xf2, 0xf2, 0xf2, 0xf2 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff3, 0xfff3, 0xfff3, 0xfff3,
					0xfff3, 0xfff3, 0xfff3, 0xfff3 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff1, 0xfffffff1,
					0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0,
					0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff0,
					 0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5,
					 0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					 0xfff1, 0xfff1, 0xfff1, 0xfff1 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xf7, 0xf7, 0xf7, 0xf7,
					    0xf7, 0xf7, 0xf7, 0xf7,
					    0xf7, 0xf7, 0xf7, 0xf7,
					    0xf7, 0xf7, 0xf7, 0xf7 };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xca80, 0xca80,
					       0xca80, 0xca80,
					       0xca80, 0xca80,
					       0xca80, 0xca80 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1700000, 0xc1700000,
					   0xc1700000, 0xc1700000 };

#define TEST_MSG "VDUP_LANE/VDUPQ_LANE"
void exec_vdup_lane (void)
{
  /* Basic test: vec1=vdup_lane(vec2, lane), then store the result.  */
#define TEST_VDUP_LANE(Q, T1, T2, W, N, N2, L)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    vdup##Q##_lane_##T2##W(VECT_VAR(vector, T1, W, N2), L);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  /* Input vector can only have 64 bits.  */
  DECL_VARIABLE_64BITS_VARIANTS(vector);

  DECL_VARIABLE_ALL_VARIANTS(vector_res);

  clean_results ();

  TEST_MACRO_64BITS_VARIANTS_2_5(VLOAD, vector, buffer);
  MFLOAT8_ONLY(VLOAD(vector, buffer, , mfloat, mf, 8, 8);)
#if defined (FP16_SUPPORTED)
  VLOAD(vector, buffer, , float, f, 16, 4);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);

  /* Choose lane arbitrarily.  */
  TEST_VDUP_LANE(, int, s, 8, 8, 8, 1);
  TEST_VDUP_LANE(, int, s, 16, 4, 4, 2);
  TEST_VDUP_LANE(, int, s, 32, 2, 2, 1);
  TEST_VDUP_LANE(, int, s, 64, 1, 1, 0);
  TEST_VDUP_LANE(, uint, u, 8, 8, 8, 7);
  TEST_VDUP_LANE(, uint, u, 16, 4, 4, 3);
  TEST_VDUP_LANE(, uint, u, 32, 2, 2, 1);
  TEST_VDUP_LANE(, uint, u, 64, 1, 1, 0);
  TEST_VDUP_LANE(, poly, p, 8, 8, 8, 7);
  TEST_VDUP_LANE(, poly, p, 16, 4, 4, 3);
  MFLOAT8_ONLY(TEST_VDUP_LANE(, mfloat, mf, 8, 8, 8, 6);)
#if defined (FP16_SUPPORTED)
  TEST_VDUP_LANE(, float, f, 16, 4, 4, 3);
#endif
  TEST_VDUP_LANE(, float, f, 32, 2, 2, 1);

  TEST_VDUP_LANE(q, int, s, 8, 16, 8, 2);
  TEST_VDUP_LANE(q, int, s, 16, 8, 4, 3);
  TEST_VDUP_LANE(q, int, s, 32, 4, 2, 1);
  TEST_VDUP_LANE(q, int, s, 64, 2, 1, 0);
  TEST_VDUP_LANE(q, uint, u, 8, 16, 8, 5);
  TEST_VDUP_LANE(q, uint, u, 16, 8, 4, 1);
  TEST_VDUP_LANE(q, uint, u, 32, 4, 2, 0);
  TEST_VDUP_LANE(q, uint, u, 64, 2, 1, 0);
  TEST_VDUP_LANE(q, poly, p, 8, 16, 8, 5);
  TEST_VDUP_LANE(q, poly, p, 16, 8, 4, 1);
  MFLOAT8_ONLY(TEST_VDUP_LANE(q, mfloat, mf, 8, 16, 8, 7);)
#if defined (FP16_SUPPORTED)
  TEST_VDUP_LANE(q, float, f, 16, 8, 4, 3);
#endif
  TEST_VDUP_LANE(q, float, f, 32, 4, 2, 1);

#if defined (FP16_SUPPORTED)
  CHECK_RESULTS (TEST_MSG, "");
#else
  CHECK_RESULTS_NO_FP16 (TEST_MSG, "");
#endif

#if defined (__aarch64__)

#undef TEST_MSG
#define TEST_MSG "VDUP_LANEQ/VDUPQ_LANEQ"

  /* Expected results for vdup*_laneq tests.  */
VECT_VAR_DECL(expected2,int,8,8) [] = { 0xfd, 0xfd, 0xfd, 0xfd,
					0xfd, 0xfd, 0xfd, 0xfd };
VECT_VAR_DECL(expected2,int,16,4) [] = { 0xfff2, 0xfff2, 0xfff2, 0xfff2 };
VECT_VAR_DECL(expected2,int,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected2,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected2,uint,8,8) [] = { 0xff, 0xff, 0xff, 0xff,
					 0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected2,uint,16,4) [] = { 0xfff3, 0xfff3, 0xfff3, 0xfff3 };
VECT_VAR_DECL(expected2,uint,32,2) [] = { 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected2,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected2,poly,8,8) [] = { 0xf7, 0xf7, 0xf7, 0xf7,
					 0xf7, 0xf7, 0xf7, 0xf7 };
VECT_VAR_DECL(expected2,poly,16,4) [] = { 0xfff3, 0xfff3, 0xfff3, 0xfff3 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,8) [] = { 0xfb, 0xfb, 0xfb, 0xfb,
					    0xfb, 0xfb, 0xfb, 0xfb };
#endif
VECT_VAR_DECL(expected2,hfloat,32,2) [] = { 0xc1700000, 0xc1700000 };
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 4) [] = { 0xca80, 0xca80,
						0xca80, 0xca80 };
#endif
VECT_VAR_DECL(expected2,int,8,16) [] = { 0xfb, 0xfb, 0xfb, 0xfb,
					 0xfb, 0xfb, 0xfb, 0xfb,
					 0xfb, 0xfb, 0xfb, 0xfb,
					 0xfb, 0xfb, 0xfb, 0xfb };
VECT_VAR_DECL(expected2,int,16,8) [] = { 0xfff7, 0xfff7, 0xfff7, 0xfff7,
					 0xfff7, 0xfff7, 0xfff7, 0xfff7 };
VECT_VAR_DECL(expected2,int,32,4) [] = { 0xfffffff1, 0xfffffff1,
					 0xfffffff1, 0xfffffff1 };
VECT_VAR_DECL(expected2,int,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff0 };
VECT_VAR_DECL(expected2,uint,8,16) [] = { 0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected2,uint,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					  0xfff1, 0xfff1, 0xfff1, 0xfff1 };
VECT_VAR_DECL(expected2,uint,32,4) [] = { 0xfffffff0, 0xfffffff0,
					  0xfffffff0, 0xfffffff0 };
VECT_VAR_DECL(expected2,uint,64,2) [] = { 0xfffffffffffffff0,
					  0xfffffffffffffff0 };
VECT_VAR_DECL(expected2,poly,8,16) [] = { 0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5,
					  0xf5, 0xf5, 0xf5, 0xf5 };
VECT_VAR_DECL(expected2,poly,16,8) [] = { 0xfff1, 0xfff1, 0xfff1, 0xfff1,
					  0xfff1, 0xfff1, 0xfff1, 0xfff1 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected2,hmfloat,8,16) [] = { 0xfc, 0xfc, 0xfc, 0xfc,
					     0xfc, 0xfc, 0xfc, 0xfc,
					     0xfc, 0xfc, 0xfc, 0xfc,
					     0xfc, 0xfc, 0xfc, 0xfc };
#endif
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected2, hfloat, 16, 8) [] = { 0xc880, 0xc880,
						0xc880, 0xc880,
						0xc880, 0xc880,
						0xc880, 0xc880 };
#endif
VECT_VAR_DECL(expected2,hfloat,32,4) [] = { 0xc1700000, 0xc1700000,
					    0xc1700000, 0xc1700000 };

  /* Clean all results for vdup*_laneq tests.  */
  clean_results ();
  /* Basic test: vec1=vdup_lane(vec2, lane), then store the result.  */
#define TEST_VDUP_LANEQ(Q, T1, T2, W, N, N2, L)				\
  VECT_VAR(vector_res, T1, W, N) =					\
    vdup##Q##_laneq_##T2##W(VECT_VAR(vector, T1, W, N2), L);		\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector_res, T1, W, N))

  /* Input vector can only have 64 bits.  */
  DECL_VARIABLE_128BITS_VARIANTS(vector);

  clean_results ();

  TEST_MACRO_128BITS_VARIANTS_2_5(VLOAD, vector, buffer);
  MFLOAT8_ONLY(VLOAD(vector, buffer, q, mfloat, mf, 8, 16);)
#if defined (FP16_SUPPORTED)
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Choose lane arbitrarily.  */
  TEST_VDUP_LANEQ(, int, s, 8, 8, 16, 13);
  TEST_VDUP_LANEQ(, int, s, 16, 4, 8, 2);
  TEST_VDUP_LANEQ(, int, s, 32, 2, 4, 1);
  TEST_VDUP_LANEQ(, int, s, 64, 1, 2, 0);
  TEST_VDUP_LANEQ(, uint, u, 8, 8, 16, 15);
  TEST_VDUP_LANEQ(, uint, u, 16, 4, 8, 3);
  TEST_VDUP_LANEQ(, uint, u, 32, 2, 4, 1);
  TEST_VDUP_LANEQ(, uint, u, 64, 1, 2, 0);
  TEST_VDUP_LANEQ(, poly, p, 8, 8, 16, 7);
  TEST_VDUP_LANEQ(, poly, p, 16, 4, 8, 3);
  MFLOAT8_ONLY(TEST_VDUP_LANEQ(, mfloat, mf, 8, 8, 16, 11);)
#if defined (FP16_SUPPORTED)
  TEST_VDUP_LANEQ(, float, f, 16, 4, 8, 3);
#endif
  TEST_VDUP_LANEQ(, float, f, 32, 2, 4, 1);

  TEST_VDUP_LANEQ(q, int, s, 8, 16, 16, 11);
  TEST_VDUP_LANEQ(q, int, s, 16, 8, 8, 7);
  TEST_VDUP_LANEQ(q, int, s, 32, 4, 4, 1);
  TEST_VDUP_LANEQ(q, int, s, 64, 2, 2, 0);
  TEST_VDUP_LANEQ(q, uint, u, 8, 16, 16, 5);
  TEST_VDUP_LANEQ(q, uint, u, 16, 8, 8, 1);
  TEST_VDUP_LANEQ(q, uint, u, 32, 4, 4, 0);
  TEST_VDUP_LANEQ(q, uint, u, 64, 2, 2, 0);
  TEST_VDUP_LANEQ(q, poly, p, 8, 16, 16, 5);
  TEST_VDUP_LANEQ(q, poly, p, 16, 8, 8, 1);
  MFLOAT8_ONLY(TEST_VDUP_LANEQ(q, mfloat, mf, 8, 16, 16, 12);)
#if defined (FP16_SUPPORTED)
  TEST_VDUP_LANEQ(q, float, f, 16, 8, 8, 7);
#endif
  TEST_VDUP_LANEQ(q, float, f, 32, 4, 4, 1);

  CHECK_RESULTS_NAMED (TEST_MSG, expected2, "");
#if defined (FP16_SUPPORTED)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected2, "");
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected2, "");
#endif

#endif /* __aarch64__.  */
}

int main (void)
{
  exec_vdup_lane ();
  return 0;
}

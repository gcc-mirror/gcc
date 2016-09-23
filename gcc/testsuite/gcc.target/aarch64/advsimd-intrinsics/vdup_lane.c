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
#if defined (FP16_SUPPORTED)
VECT_VAR_DECL (expected, hfloat, 16, 8) [] = { 0xca80, 0xca80,
					       0xca80, 0xca80,
					       0xca80, 0xca80,
					       0xca80, 0xca80 };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1700000, 0xc1700000,
					   0xc1700000, 0xc1700000 };

#define TEST_MSG "VDUP_LANE/VDUP_LANEQ"
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
#if defined (FP16_SUPPORTED)
  TEST_VDUP_LANE(q, float, f, 16, 8, 4, 3);
#endif
  TEST_VDUP_LANE(q, float, f, 32, 4, 2, 1);

#if defined (FP16_SUPPORTED)
  CHECK_RESULTS (TEST_MSG, "");
#else
  CHECK_RESULTS_NO_FP16 (TEST_MSG, "");
#endif
}

int main (void)
{
  exec_vdup_lane ();
  return 0;
}

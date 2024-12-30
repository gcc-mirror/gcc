#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
				       0xf4, 0xf5, 0xf6, 0x11 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0x22 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0x33 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0x44 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0x55, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff1, 0x66, 0xfff3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0x77 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0x88 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0x55, 0xf7 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff0, 0xfff1, 0x66, 0xfff3 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					   0xbb, 0xf5, 0xf6, 0xf7 };
#endif
VECT_VAR_DECL(expected,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0x4840, 0xca80 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800000, 0x4204cccd };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7,
					0xf8, 0xf9, 0xfa, 0xfb,
					0xfc, 0xfd, 0xfe, 0x99 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff4, 0xaa, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					0xfffffff2, 0xbb };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0, 0xcc };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xf8, 0xf9, 0xfa, 0xfb,
					 0xfc, 0xfd, 0xdd, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0xfff4, 0xfff5, 0xee, 0xfff7 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					 0xff, 0xfffffff3 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0, 0x11 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xf8, 0xf9, 0xfa, 0xfb,
					 0xfc, 0xfd, 0xdd, 0xff };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0xfff4, 0xfff5, 0xee, 0xfff7 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					    0xf4, 0xf5, 0xf6, 0xf7,
					    0xf8, 0xf9, 0xa0, 0xfb,
					    0xfc, 0xfd, 0xfe, 0xff };
#endif
VECT_VAR_DECL(expected,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
					   0xca00, 0x4480, 0xc900, 0xc880 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					   0xc1600000, 0x41333333 };

#define TEST_MSG "VSET_LANE/VSET_LANEQ"
void exec_vset_lane (void)
{
  /* vec=vset_lane(val, vec, lane), then store the result.  */
#define TEST_VSET_LANE(Q, T1, T2, W, N, V, L)				\
  VECT_VAR(vector, T1, W, N) =						\
    vset##Q##_lane_##T2##W(V,						\
			   VECT_VAR(vector, T1, W, N),			\
			   L);						\
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);

  clean_results ();

  /* Initialize input "vector" from "buffer".  */
  TEST_MACRO_ALL_VARIANTS_2_5(VLOAD, vector, buffer);
#if MFLOAT8_SUPPORTED
  VLOAD (vector, buffer, , mfloat, mf, 8, 8);
  VLOAD (vector, buffer, q, mfloat, mf, 8, 16);
#endif
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vector, buffer, , float, f, 16, 4);
  VLOAD(vector, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector, buffer, , float, f, 32, 2);
  VLOAD(vector, buffer, q, float, f, 32, 4);

  /* Choose value and lane arbitrarily.  */
  TEST_VSET_LANE(, int, s, 8, 8, 0x11, 7);
  TEST_VSET_LANE(, int, s, 16, 4, 0x22, 3);
  TEST_VSET_LANE(, int, s, 32, 2, 0x33, 1);
  TEST_VSET_LANE(, int, s, 64, 1, 0x44, 0);
  TEST_VSET_LANE(, uint, u, 8, 8, 0x55, 6);
  TEST_VSET_LANE(, uint, u, 16, 4, 0x66, 2);
  TEST_VSET_LANE(, uint, u, 32, 2, 0x77, 1);
  TEST_VSET_LANE(, uint, u, 64, 1, 0x88, 0);
  TEST_VSET_LANE(, poly, p, 8, 8, 0x55, 6);
  TEST_VSET_LANE(, poly, p, 16, 4, 0x66, 2);
  MFLOAT8_ONLY(TEST_VSET_LANE(, mfloat, mf, 8, 8, MFLOAT8(0xbb), 4));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VSET_LANE(, float, f, 16, 4, 8.5f, 2);
#endif
  TEST_VSET_LANE(, float, f, 32, 2, 33.2f, 1);

  TEST_VSET_LANE(q, int, s, 8, 16, 0x99, 15);
  TEST_VSET_LANE(q, int, s, 16, 8, 0xAA, 5);
  TEST_VSET_LANE(q, int, s, 32, 4, 0xBB, 3);
  TEST_VSET_LANE(q, int, s, 64, 2, 0xCC, 1);
  TEST_VSET_LANE(q, uint, u, 8, 16, 0xDD, 14);
  TEST_VSET_LANE(q, uint, u, 16, 8, 0xEE, 6);
  TEST_VSET_LANE(q, uint, u, 32, 4, 0xFF, 2);
  TEST_VSET_LANE(q, uint, u, 64, 2, 0x11, 1);
  TEST_VSET_LANE(q, poly, p, 8, 16, 0xDD, 14);
  TEST_VSET_LANE(q, poly, p, 16, 8, 0xEE, 6);
  MFLOAT8_ONLY(TEST_VSET_LANE(q, mfloat, mf, 8, 16, MFLOAT8(0xa0), 10));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VSET_LANE(q, float, f, 16, 8, 4.5f, 5);
#endif
  TEST_VSET_LANE(q, float, f, 32, 4, 11.2f, 3);

  CHECK_RESULTS(TEST_MSG, "");
}

int main (void)
{
  exec_vset_lane ();
  return 0;
}

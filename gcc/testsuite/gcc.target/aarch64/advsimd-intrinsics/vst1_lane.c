#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf7, 0x33, 0x33, 0x33,
				       0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff3, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff1, 0x33333333 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf6, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff2, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf6, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff2, 0x3333, 0x3333, 0x3333 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,8) [] = { 0xf2, 0x33, 0x33, 0x33,
					   0x33, 0x33, 0x33, 0x33 };
#endif
VECT_VAR_DECL(expected,hfloat,16,4) [] = { 0xcb80, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1700000, 0x33333333 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xff, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff5, 0x3333, 0x3333, 0x3333,
					0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff1, 0x33333333,
					0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff1, 0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xfa, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff4, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff3, 0x33333333,
					 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xfa, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff4, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xfe, 0x33, 0x33, 0x33,
					    0x33, 0x33, 0x33, 0x33,
					    0x33, 0x33, 0x33, 0x33,
					    0x33, 0x33, 0x33, 0x33 };
#endif
VECT_VAR_DECL(expected,hfloat,16,8) [] = { 0xc900, 0x3333, 0x3333, 0x3333,
					   0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1700000, 0x33333333,
					   0x33333333, 0x33333333 };

#define TEST_MSG "VST1_LANE/VST1_LANEQ"
void exec_vst1_lane (void)
{
#define TEST_VST1_LANE(Q, T1, T2, W, N, L)		\
  VECT_VAR(vector, T1, W, N) =				\
    vld1##Q##_##T2##W(VECT_VAR(buffer, T1, W, N));	\
  vst1##Q##_lane_##T2##W(VECT_VAR(result, T1, W, N),	\
			 VECT_VAR(vector, T1, W, N), L)

  DECL_VARIABLE_ALL_VARIANTS(vector);

  clean_results ();

  /* Choose lane arbitrarily.  */
  TEST_VST1_LANE(, int, s, 8, 8, 7);
  TEST_VST1_LANE(, int, s, 16, 4, 3);
  TEST_VST1_LANE(, int, s, 32, 2, 1);
  TEST_VST1_LANE(, int, s, 64, 1, 0);
  TEST_VST1_LANE(, uint, u, 8, 8, 6);
  TEST_VST1_LANE(, uint, u, 16, 4, 2);
  TEST_VST1_LANE(, uint, u, 32, 2, 0);
  TEST_VST1_LANE(, uint, u, 64, 1, 0);
  TEST_VST1_LANE(, poly, p, 8, 8, 6);
  TEST_VST1_LANE(, poly, p, 16, 4, 2);
  MFLOAT8_ONLY(TEST_VST1_LANE(, mfloat, mf, 8, 8, 2));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VST1_LANE(, float, f, 16, 4, 1);
#endif
  TEST_VST1_LANE(, float, f, 32, 2, 1);

  TEST_VST1_LANE(q, int, s, 8, 16, 15);
  TEST_VST1_LANE(q, int, s, 16, 8, 5);
  TEST_VST1_LANE(q, int, s, 32, 4, 1);
  TEST_VST1_LANE(q, int, s, 64, 2, 1);
  TEST_VST1_LANE(q, uint, u, 8, 16, 10);
  TEST_VST1_LANE(q, uint, u, 16, 8, 4);
  TEST_VST1_LANE(q, uint, u, 32, 4, 3);
  TEST_VST1_LANE(q, uint, u, 64, 2, 0);
  TEST_VST1_LANE(q, poly, p, 8, 16, 10);
  TEST_VST1_LANE(q, poly, p, 16, 8, 4);
  MFLOAT8_ONLY(TEST_VST1_LANE(q, mfloat, mf, 8, 16, 14));
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VST1_LANE(q, float, f, 16, 8, 6);
#endif
  TEST_VST1_LANE(q, float, f, 32, 4, 1);

  CHECK_RESULTS(TEST_MSG, "");
}

int main (void)
{
  exec_vst1_lane ();
  return 0;
}

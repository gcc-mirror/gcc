#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
				       0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff0 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected,hfloat,16,4) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1800000, 0xc1700000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7,
					0xf8, 0xf9, 0xfa, 0xfb,
					0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1,
					0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0,
					0xfffffffffffffff1 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xf8, 0xf9, 0xfa, 0xfb,
					 0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2,
					 0xfff3, 0xfff4, 0xfff5,
					 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff1,
					 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0,
					 0xfffffffffffffff1 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0xf8, 0xf9, 0xfa, 0xfb,
					 0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
					   0xca00, 0xc980, 0xc900, 0xc880 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					   0xc1600000, 0xc1500000 };

#define TEST_MSG "VLD1/VLD1Q"
void exec_vld1 (void)
{
  /* Basic test vec=vld1(buffer); then store vec: vst1(result, vector).  */
  /* This test actually tests vdl1 and vst1 at the same time.  */
#define TEST_VLD1(VAR, BUF, Q, T1, T2, W, N)				\
  VECT_VAR(VAR, T1, W, N) = vld1##Q##_##T2##W(VECT_VAR(BUF, T1, W, N)); \
  vst1##Q##_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(VAR, T1, W, N))

  DECL_VARIABLE_ALL_VARIANTS(vector);

  clean_results ();

  TEST_MACRO_ALL_VARIANTS_2_5(TEST_VLD1, vector, buffer);

#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VLD1(vector, buffer, , float, f, 16, 4);
  TEST_VLD1(vector, buffer, q, float, f, 16, 8);
#endif
  TEST_VLD1(vector, buffer, , float, f, 32, 2);
  TEST_VLD1(vector, buffer, q, float, f, 32, 4);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  exec_vld1 ();
  return 0;
}

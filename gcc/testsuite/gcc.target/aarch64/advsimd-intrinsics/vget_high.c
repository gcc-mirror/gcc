#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
				       0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,int,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,int,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected,int,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected,uint,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,uint,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,uint,32,2) [] = { 0xfffffff2, 0xfffffff3 };
VECT_VAR_DECL(expected,uint,64,1) [] = { 0xfffffffffffffff1 };
VECT_VAR_DECL(expected,poly,8,8) [] = { 0xf8, 0xf9, 0xfa, 0xfb,
					0xfc, 0xfd, 0xfe, 0xff };
VECT_VAR_DECL(expected,poly,16,4) [] = { 0xfff4, 0xfff5, 0xfff6, 0xfff7 };
VECT_VAR_DECL(expected,hfloat,32,2) [] = { 0xc1600000, 0xc1500000 };
VECT_VAR_DECL(expected,int,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33,
					0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x33333333, 0x33333333,
					0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x3333333333333333,
					0x3333333333333333 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x33333333, 0x33333333,
					 0x33333333, 0x33333333 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x3333333333333333,
					 0x3333333333333333 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33,
					 0x33, 0x33, 0x33, 0x33 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x3333, 0x3333, 0x3333, 0x3333,
					 0x3333, 0x3333, 0x3333, 0x3333 };
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0x33333333, 0x33333333,
					   0x33333333, 0x33333333 };

#define TEST_MSG "VGET_HIGH"
void exec_vget_high (void)
{
  /* Basic test: vec64=vget_high(vec128), then store the result.  */
#define TEST_VGET_HIGH(T1, T2, W, N, N2)				\
  VECT_VAR(vector64, T1, W, N) =					\
    vget_high_##T2##W(VECT_VAR(vector128, T1, W, N2));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector64, T1, W, N))

  DECL_VARIABLE_64BITS_VARIANTS(vector64);
  DECL_VARIABLE_128BITS_VARIANTS(vector128);

  TEST_MACRO_128BITS_VARIANTS_2_5(VLOAD, vector128, buffer);
  VLOAD(vector128, buffer, q, float, f, 32, 4);

  clean_results ();

  /* Execute the tests.  */
  TEST_VGET_HIGH(int, s, 8, 8, 16);
  TEST_VGET_HIGH(int, s, 16, 4, 8);
  TEST_VGET_HIGH(int, s, 32, 2, 4);
  TEST_VGET_HIGH(int, s, 64, 1, 2);
  TEST_VGET_HIGH(uint, u, 8, 8, 16);
  TEST_VGET_HIGH(uint, u, 16, 4, 8);
  TEST_VGET_HIGH(uint, u, 32, 2, 4);
  TEST_VGET_HIGH(uint, u, 64, 1, 2);
  TEST_VGET_HIGH(poly, p, 8, 8, 16);
  TEST_VGET_HIGH(poly, p, 16, 4, 8);
  TEST_VGET_HIGH(float, f, 32, 2, 4);

  CHECK_RESULTS (TEST_MSG, "");
}

int main (void)
{
  exec_vget_high ();
  return 0;
}

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

#define TEST_MSG "VGET_LOW"
void exec_vget_low (void)
{
  /* Basic test: vec64=vget_low(vec128), then store the result.  */
#define TEST_VGET_LOW(T1, T2, W, N, N2)					\
  VECT_VAR(vector64, T1, W, N) =					\
    vget_low_##T2##W(VECT_VAR(vector128, T1, W, N2));			\
  vst1_##T2##W(VECT_VAR(result, T1, W, N), VECT_VAR(vector64, T1, W, N))

  DECL_VARIABLE_64BITS_VARIANTS(vector64);
  DECL_VARIABLE_128BITS_VARIANTS(vector128);

  TEST_MACRO_128BITS_VARIANTS_2_5(VLOAD, vector128, buffer);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vector128, buffer, q, float, f, 16, 8);
#endif
  VLOAD(vector128, buffer, q, float, f, 32, 4);

  clean_results ();

  /* Execute the tests.  */
  TEST_VGET_LOW(int, s, 8, 8, 16);
  TEST_VGET_LOW(int, s, 16, 4, 8);
  TEST_VGET_LOW(int, s, 32, 2, 4);
  TEST_VGET_LOW(int, s, 64, 1, 2);
  TEST_VGET_LOW(uint, u, 8, 8, 16);
  TEST_VGET_LOW(uint, u, 16, 4, 8);
  TEST_VGET_LOW(uint, u, 32, 2, 4);
  TEST_VGET_LOW(uint, u, 64, 1, 2);
  TEST_VGET_LOW(poly, p, 8, 8, 16);
  TEST_VGET_LOW(poly, p, 16, 4, 8);
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VGET_LOW(float, f, 16, 4, 8);
#endif
  TEST_VGET_LOW(float, f, 32, 2, 4);

  CHECK(TEST_MSG, int, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 1, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 8, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 4, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 2, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 1, PRIx64, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 8, PRIx8, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 4, PRIx16, expected, "");
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 4, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 2, PRIx32, expected, "");
}

int main (void)
{
  exec_vget_low ();
  return 0;
}

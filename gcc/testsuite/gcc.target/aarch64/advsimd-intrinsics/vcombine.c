#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					0xf4, 0xf5, 0xf6, 0xf7,
					0x11, 0x11, 0x11, 0x11,
					0x11, 0x11, 0x11, 0x11 };
VECT_VAR_DECL(expected,int,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					0x22, 0x22, 0x22, 0x22 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0xfffffff0, 0xfffffff1, 0x33, 0x33 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0xfffffffffffffff0, 0x44 };
VECT_VAR_DECL(expected,uint,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0x66, 0x66, 0x66, 0x66 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xfffffff0, 0xfffffff1, 0x77, 0x77 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xfffffffffffffff0, 0x88 };
VECT_VAR_DECL(expected,poly,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					 0xf4, 0xf5, 0xf6, 0xf7,
					 0x55, 0x55, 0x55, 0x55,
					 0x55, 0x55, 0x55, 0x55 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0xfff0, 0xfff1, 0xfff2, 0xfff3,
					 0x66, 0x66, 0x66, 0x66 };
#if MFLOAT8_SUPPORTED
VECT_VAR_DECL(expected,hmfloat,8,16) [] = { 0xf0, 0xf1, 0xf2, 0xf3,
					    0xf4, 0xf5, 0xf6, 0xf7,
					    0xcc, 0xcc, 0xcc, 0xcc,
					    0xcc, 0xcc, 0xcc, 0xcc };
#endif
VECT_VAR_DECL(expected,hfloat,32,4) [] = { 0xc1800000, 0xc1700000,
					   0x40533333, 0x40533333 };
VECT_VAR_DECL(expected,hfloat,16,8) [] = { 0xcc00, 0xcb80, 0xcb00, 0xca80,
					   0x4080, 0x4080, 0x4080, 0x4080 };

#define TEST_MSG "VCOMBINE"
void exec_vcombine (void)
{
  /* Basic test: vec128=vcombine(vec64_a, vec64_b), then store the result.  */
#define TEST_VCOMBINE(T1, T2, W, N, N2)					\
  VECT_VAR(vector128, T1, W, N2) =					\
    vcombine_##T2##W(VECT_VAR(vector64_a, T1, W, N),			\
		     VECT_VAR(vector64_b, T1, W, N));			\
  vst1q_##T2##W(VECT_VAR(result, T1, W, N2), VECT_VAR(vector128, T1, W, N2))

  DECL_VARIABLE_64BITS_VARIANTS(vector64_a);
  DECL_VARIABLE_64BITS_VARIANTS(vector64_b);
  DECL_VARIABLE_128BITS_VARIANTS(vector128);

  /* Initialize input "vector64_a" from "buffer".  */
  TEST_MACRO_64BITS_VARIANTS_2_5(VLOAD, vector64_a, buffer);
  MFLOAT8_ONLY(VLOAD(vector64_a, buffer, , mfloat, mf, 8, 8);)
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VLOAD(vector64_a, buffer, , float, f, 16, 4);
#endif
  VLOAD(vector64_a, buffer, , float, f, 32, 2);

  /* Choose init value arbitrarily.  */
  VDUP(vector64_b, , int, s, 8, 8, 0x11);
  VDUP(vector64_b, , int, s, 16, 4, 0x22);
  VDUP(vector64_b, , int, s, 32, 2, 0x33);
  VDUP(vector64_b, , int, s, 64, 1, 0x44);
  VDUP(vector64_b, , uint, u, 8, 8, 0x55);
  VDUP(vector64_b, , uint, u, 16, 4, 0x66);
  VDUP(vector64_b, , uint, u, 32, 2, 0x77);
  VDUP(vector64_b, , uint, u, 64, 1, 0x88);
  VDUP(vector64_b, , poly, p, 8, 8, 0x55);
  VDUP(vector64_b, , poly, p, 16, 4, 0x66);
  MFLOAT8_ONLY(VDUP(vector64_b, , mfloat, mf, 8, 8, MFLOAT8(0xcc));)
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  VDUP(vector64_b, , float, f, 16, 4, 2.25);
#endif
  VDUP(vector64_b, , float, f, 32, 2, 3.3f);

  clean_results ();

  /* Execute the tests.  */
  TEST_VCOMBINE(int, s, 8, 8, 16);
  TEST_VCOMBINE(int, s, 16, 4, 8);
  TEST_VCOMBINE(int, s, 32, 2, 4);
  TEST_VCOMBINE(int, s, 64, 1, 2);
  TEST_VCOMBINE(uint, u, 8, 8, 16);
  TEST_VCOMBINE(uint, u, 16, 4, 8);
  TEST_VCOMBINE(uint, u, 32, 2, 4);
  TEST_VCOMBINE(uint, u, 64, 1, 2);
  TEST_VCOMBINE(poly, p, 8, 8, 16);
  TEST_VCOMBINE(poly, p, 16, 4, 8);
  MFLOAT8_ONLY(TEST_VCOMBINE(mfloat, mf, 8, 8, 16);)
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  TEST_VCOMBINE(float, f, 16, 4, 8);
#endif
  TEST_VCOMBINE(float, f, 32, 2, 4);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
  CHECK_POLY(TEST_MSG, poly, 8, 16, PRIx8, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected, "");
  MFLOAT8_ONLY(CHECK_FP(TEST_MSG, mfloat, 8, 16, PRIx16, expected, "");)
#if defined (__ARM_FP16_FORMAT_IEEE) || defined (__ARM_FP16_FORMAT_ALTERNATIVE)
  CHECK_FP(TEST_MSG, float, 16, 8, PRIx16, expected, "");
#endif
  CHECK_FP(TEST_MSG, float, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  exec_vcombine ();
  return 0;
}

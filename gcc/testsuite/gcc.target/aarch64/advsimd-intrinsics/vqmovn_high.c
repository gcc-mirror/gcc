/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					   0x5, 0x5, 0x5, 0x5,
					   0x12, 0x12, 0x12, 0x12,
					   0x12, 0x12, 0x12, 0x12 };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					   0x1278, 0x1278, 0x1278, 0x1278 };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x5, 0x5, 0x12345678, 0x12345678 };
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0x82, 0x82, 0x82, 0x82,
					    0x82, 0x82, 0x82, 0x82 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x8765, 0x8765, 0x8765, 0x8765 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x5, 0x5, 0x87654321, 0x87654321 };

/* Expected results when saturation occurs.  */
VECT_VAR_DECL(expected1, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0x7f, 0x7f, 0x7f, 0x7f,
					    0x7f, 0x7f, 0x7f, 0x7f };
VECT_VAR_DECL(expected1, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x7fff, 0x7fff, 0x7fff, 0x7fff };
VECT_VAR_DECL(expected1, int, 32, 4) [] = { 0x5, 0x5, 0x7fffffff, 0x7fffffff };
VECT_VAR_DECL(expected1, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					     0x5, 0x5, 0x5, 0x5,
					     0xff, 0xff, 0xff, 0xff,
					     0xff, 0xff, 0xff, 0xff };
VECT_VAR_DECL(expected1, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					     0xffff, 0xffff, 0xffff, 0xffff };
VECT_VAR_DECL(expected1, uint, 32, 4) [] = { 0x5, 0x5, 
					     0xffffffff, 0xffffffff };

#define TEST_MSG "VQMOVN_HIGH"
void exec_vqmovn_high (void)
{
  /* Basic test: vec128_r=vqmovn_high(vec64_r,vec128_x), store the result.  */
#define TEST_VQMOVN_HIGH(T1, T2, W1, W2, N1, N2, CMT)				\
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vec128_r, T1, W1, N2));			\
  VECT_VAR(vec128_r, T1, W1, N2) =						\
    vqmovn_high_##T2##W2(VECT_VAR(vec64_r, T1, W1, N1),				\
			 VECT_VAR(vec128_x, T1, W2, N1));			\
  vst1q##_##T2##W1(VECT_VAR(result, T1, W1, N2),				\
		   VECT_VAR(vec128_r, T1, W1, N2))

  DECL_VARIABLE_128BITS_VARIANTS(vec128_r);
  DECL_VARIABLE_64BITS_VARIANTS(vec64_r);
  DECL_VARIABLE_128BITS_VARIANTS(vec128_x);

  clean_results ();

  /* Fill vec64_r with a value easy to recognise in the result vector. */
  VDUP(vec64_r, , int, s, 8, 8, 0x5);
  VDUP(vec64_r, , int, s, 16, 4, 0x5);
  VDUP(vec64_r, , int, s, 32, 2, 0x5);
  VDUP(vec64_r, , uint, u, 8, 8, 0x5);
  VDUP(vec64_r, , uint, u, 16, 4, 0x5);
  VDUP(vec64_r, , uint, u, 32, 2, 0x5);

  /* Fill input vector with arbitrary values.  */
  VDUP(vec128_x, q, int, s, 16, 8, 0x12);
  VDUP(vec128_x, q, int, s, 32, 4, 0x1278);
  VDUP(vec128_x, q, int, s, 64, 2, 0x12345678);
  VDUP(vec128_x, q, uint, u, 16, 8, 0x82);
  VDUP(vec128_x, q, uint, u, 32, 4, 0x8765);
  VDUP(vec128_x, q, uint, u, 64, 2, 0x87654321);


#define CMT ""
  TEST_VQMOVN_HIGH(int, s, 8, 16, 8, 16, CMT);
  TEST_VQMOVN_HIGH(int, s, 16, 32, 4, 8, CMT);
  TEST_VQMOVN_HIGH(int, s, 32, 64, 2, 4, CMT);
  TEST_VQMOVN_HIGH(uint, u, 8, 16, 8, 16, CMT);
  TEST_VQMOVN_HIGH(uint, u, 16, 32, 4, 8, CMT);
  TEST_VQMOVN_HIGH(uint, u, 32, 64, 2, 4, CMT);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);


  /* Fill input vector with arbitrary values which cause cumulative
     saturation.  */
  VDUP(vec128_x, q, int, s, 16, 8, 0x1234);
  VDUP(vec128_x, q, int, s, 32, 4, 0x12345678);
  VDUP(vec128_x, q, int, s, 64, 2, 0x1234567890ABLL);
  VDUP(vec128_x, q, uint, u, 16, 8, 0x8234);
  VDUP(vec128_x, q, uint, u, 32, 4, 0x87654321);
  VDUP(vec128_x, q, uint, u, 64, 2, 0x8765432187654321ULL);

#undef CMT
#define CMT " (with saturation)"
  TEST_VQMOVN_HIGH(int, s, 8, 16, 8, 16, CMT);
  TEST_VQMOVN_HIGH(int, s, 16, 32, 4, 8, CMT);
  TEST_VQMOVN_HIGH(int, s, 32, 64, 2, 4, CMT);
  TEST_VQMOVN_HIGH(uint, u, 8, 16, 8, 16, CMT);
  TEST_VQMOVN_HIGH(uint, u, 16, 32, 4, 8, CMT);
  TEST_VQMOVN_HIGH(uint, u, 32, 64, 2, 4, CMT);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected1, CMT);
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected1, CMT);
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected1, CMT);
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected1, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected1, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected1, CMT);
}

int main (void)
{
  exec_vqmovn_high ();
  return 0;
}

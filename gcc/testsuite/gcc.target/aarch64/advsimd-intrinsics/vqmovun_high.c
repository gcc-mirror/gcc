/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0x34, 0x34, 0x34, 0x34,
					    0x34, 0x34, 0x34, 0x34 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5678, 0x5678, 0x5678, 0x5678 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x5, 0x5, 0x12345678, 0x12345678 };

/* Expected results with negative input.  */
VECT_VAR_DECL(expected_neg, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
						0x5, 0x5, 0x5, 0x5,
						0x0, 0x0, 0x0, 0x0,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
						0x0, 0x0, 0x0, 0x0 };
VECT_VAR_DECL(expected_neg, uint, 32, 4) [] = { 0x5, 0x5, 0x0, 0x0 };

#define TEST_MSG "VQMOVUN_HIGH"
void exec_vqmovun_high (void)
{
  /* Basic test: vec128_r=vqmovun_high(vec64)_r, vec128_x), store result.  */
#define TEST_VQMOVUN_HIGH(T1, T2, W1, W2, N1, N2, CMT)				\
  Set_Neon_Cumulative_Sat(0, VECT_VAR(vec128_r, T1, W1, N2));			\
  VECT_VAR(vec128_r, T1, W1, N2) =						\
    vqmovun_high_s##W2(VECT_VAR(vec64_r, uint, W1, N1),				\
		       VECT_VAR(vec128_x, int, W2, N1));			\
  vst1q##_u##W1(VECT_VAR(result, T1, W1, N2), VECT_VAR(vec128_r, T1, W1, N2))

  DECL_VARIABLE(vec64_r, uint, 8, 8);
  DECL_VARIABLE(vec64_r, uint, 16, 4);
  DECL_VARIABLE(vec64_r, uint, 32, 2);

  DECL_VARIABLE(vec128_x, int, 16, 8);
  DECL_VARIABLE(vec128_x, int, 32, 4);
  DECL_VARIABLE(vec128_x, int, 64, 2);

  DECL_VARIABLE(vec128_r, uint, 8, 16);
  DECL_VARIABLE(vec128_r, uint, 16, 8);
  DECL_VARIABLE(vec128_r, uint, 32, 4);

  clean_results ();

  /* Fill vec64_r with a value easy to recognise in the result vector. */
  VDUP(vec64_r, , uint, u, 8, 8, 0x5);
  VDUP(vec64_r, , uint, u, 16, 4, 0x5);
  VDUP(vec64_r, , uint, u, 32, 2, 0x5);

  /* Fill input vector with arbitrary values.  */
  VDUP(vec128_x, q, int, s, 16, 8, 0x34);
  VDUP(vec128_x, q, int, s, 32, 4, 0x5678);
  VDUP(vec128_x, q, int, s, 64, 2, 0x12345678);


#define CMT ""
  TEST_VQMOVUN_HIGH(uint, u, 8, 16, 8, 16, CMT);
  TEST_VQMOVUN_HIGH(uint, u, 16, 32, 4, 8, CMT);
  TEST_VQMOVUN_HIGH(uint, u, 32, 64, 2, 4, CMT);

  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, CMT);

  /* Fill input vector with negative values.  */
  VDUP(vec128_x, q, int, s, 16, 8, 0x8234);
  VDUP(vec128_x, q, int, s, 32, 4, 0x87654321);
  VDUP(vec128_x, q, int, s, 64, 2, 0x8765432187654321LL);


#undef CMT
#define CMT " (negative input)"
  TEST_VQMOVUN_HIGH(uint, u, 8, 16, 8, 16, CMT);
  TEST_VQMOVUN_HIGH(uint, u, 16, 32, 4, 8, CMT);
  TEST_VQMOVUN_HIGH(uint, u, 32, 64, 2, 4, CMT);

  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected_neg, CMT);
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected_neg, CMT);
}

int main (void)
{
  exec_vqmovun_high ();
  return 0;
}

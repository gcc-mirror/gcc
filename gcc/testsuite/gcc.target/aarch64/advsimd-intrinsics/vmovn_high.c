/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					   0x5, 0x5, 0x5, 0x5,
					   0xf0, 0xf1, 0xf2, 0xf3,
				           0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5, 
					   0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x5, 0x5, 0xfffffff0, 0xfffffff1 };
VECT_VAR_DECL(expected, uint, 8, 16) [] = { 0x5, 0x5, 0x5, 0x5,
					    0x5, 0x5, 0x5, 0x5,
					    0xf0, 0xf1, 0xf2, 0xf3,
					    0xf4, 0xf5, 0xf6, 0xf7 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0x5, 0x5, 0x5, 0x5,
					    0xfff0, 0xfff1, 0xfff2, 0xfff3 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x5, 0x5, 0xfffffff0, 0xfffffff1 };

#define TEST_MSG "VMOVN_HIGH"
void exec_vmovn_high (void)
{
  /* Basic test: vec128_r=vmovn_high(vec64_r, vec128_x), store the result.  */
#define TEST_VMOVN_HIGH(T1, T2, W1, W2, N1, N2)					\
  VECT_VAR(vec128_r, T1, W2, N2) =						\
    vmovn_high_##T2##W1(VECT_VAR(vec64_r, T1, W2, N1),				\
			VECT_VAR(vec128_x, T1, W1, N1));			\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2), VECT_VAR(vec128_r, T1, W2, N2))

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

  VLOAD(vec128_x, buffer, q, int, s, 16, 8);
  VLOAD(vec128_x, buffer, q, int, s, 32, 4);
  VLOAD(vec128_x, buffer, q, int, s, 64, 2);
  VLOAD(vec128_x, buffer, q, uint, u, 16, 8);
  VLOAD(vec128_x, buffer, q, uint, u, 32, 4);
  VLOAD(vec128_x, buffer, q, uint, u, 64, 2);

  TEST_VMOVN_HIGH(int, s, 16, 8, 8, 16);
  TEST_VMOVN_HIGH(int, s, 32, 16, 4, 8);
  TEST_VMOVN_HIGH(int, s, 64, 32, 2, 4);
  TEST_VMOVN_HIGH(uint, u, 16, 8, 8, 16);
  TEST_VMOVN_HIGH(uint, u, 32, 16, 4, 8);
  TEST_VMOVN_HIGH(uint, u, 64, 32, 2, 4);

  CHECK(TEST_MSG, int, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 8, 16, PRIx8, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
}

int main (void)
{
  exec_vmovn_high ();
  return 0;
}

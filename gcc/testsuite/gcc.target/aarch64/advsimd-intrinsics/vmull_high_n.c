/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x11000, 0x11000,
					   0x11000, 0x11000 };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0x22000, 0x22000 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x33000, 0x33000,
					    0x33000, 0x33000 };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0x44000, 0x44000 };

#define TEST_MSG "VMULL_HIGH_N"
void exec_vmull_high_n (void)
{
  int i;

  /* vector_res = vmull_high_n(vector, val), then store the result.  */
#define TEST_VMULL_HIGH_N(T1, T2, W1, W2, N1, N2, L)			  \
  VECT_VAR(vector_res, T1, W2, N2) =					  \
    vmull_high_n_##T2##W1(VECT_VAR(vector, T1, W1, N1), L);		  \
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2),				  \
		 VECT_VAR(vector_res, T1, W2, N2))

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);

  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  /* Initialize vector.  */
  VDUP(vector, q, int, s, 16, 8, 0x1000);
  VDUP(vector, q, int, s, 32, 4, 0x1000);
  VDUP(vector, q, uint, u, 16, 8, 0x1000);
  VDUP(vector, q, uint, u, 32, 4, 0x1000);

  /* Choose multiplier arbitrarily.  */
  TEST_VMULL_HIGH_N(int, s, 16, 32, 8, 4, 0x11);
  TEST_VMULL_HIGH_N(int, s, 32, 64, 4, 2, 0x22);
  TEST_VMULL_HIGH_N(uint, u, 16, 32, 8, 4, 0x33);
  TEST_VMULL_HIGH_N(uint, u, 32, 64, 4, 2, 0x44);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vmull_high_n ();
  return 0;
}

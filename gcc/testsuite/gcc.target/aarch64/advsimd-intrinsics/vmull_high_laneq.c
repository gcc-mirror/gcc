/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0x2000, 0x2000 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0x2000, 0x2000 };

#define TEST_MSG "VMULL_HIGH_LANEQ"
void exec_vmull_high_laneq (void)
{
  /* vector_res = vmull_high_laneq(vector, vector2, lane), store the result. */
#define TEST_VMULL_HIGH_LANEQ(T1, T2, W1, W2, N1, N2, L)		  \
  VECT_VAR(vector_res, T1, W2, N2) =					  \
    vmull_high_laneq_##T2##W1(VECT_VAR(vector, T1, W1, N1),		  \
			      VECT_VAR(vector2, T1, W1, N1),		  \
			      L);					  \
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N2),				  \
		 VECT_VAR(vector_res, T1, W2, N2))

  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector2, int, 16, 8);
  DECL_VARIABLE(vector2, int, 32, 4);
  DECL_VARIABLE(vector2, uint, 16, 8);
  DECL_VARIABLE(vector2, uint, 32, 4);

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

  /* Initialize vector2.  */
  VDUP(vector2, q, int, s, 16, 8, 0x4);
  VDUP(vector2, q, int, s, 32, 4, 0x2);
  VDUP(vector2, q, uint, u, 16, 8, 0x4);
  VDUP(vector2, q, uint, u, 32, 4, 0x2);

  /* Choose lane arbitrarily.  */
  TEST_VMULL_HIGH_LANEQ(int, s, 16, 32, 8, 4, 5);
  TEST_VMULL_HIGH_LANEQ(int, s, 32, 64, 4, 2, 1);
  TEST_VMULL_HIGH_LANEQ(uint, u, 16, 32, 8, 4, 7);
  TEST_VMULL_HIGH_LANEQ(uint, u, 32, 64, 4, 2, 3);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vmull_high_laneq ();
  return 0;
}

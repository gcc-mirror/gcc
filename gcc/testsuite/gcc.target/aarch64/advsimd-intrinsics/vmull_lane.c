#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x2000, 0x2000 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x4000, 0x4000, 0x4000, 0x4000 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x2000, 0x2000 };

#define TEST_MSG "VMULL_LANE"
void exec_vmull_lane (void)
{
  /* vector_res = vmull_lane(vector,vector2,lane), then store the result.  */
#define TEST_VMULL_LANE(T1, T2, W, W2, N, L)				\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vmull##_lane_##T2##W(VECT_VAR(vector, T1, W, N),			\
			 VECT_VAR(vector2, T1, W, N),			\
			 L);						\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector2, int, 16, 4);
  DECL_VARIABLE(vector2, int, 32, 2);
  DECL_VARIABLE(vector2, uint, 16, 4);
  DECL_VARIABLE(vector2, uint, 32, 2);

  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);

  clean_results ();

  /* Initialize vector.  */
  VDUP(vector, , int, s, 16, 4, 0x1000);
  VDUP(vector, , int, s, 32, 2, 0x1000);
  VDUP(vector, , uint, u, 16, 4, 0x1000);
  VDUP(vector, , uint, u, 32, 2, 0x1000);

  /* Initialize vector2.  */
  VDUP(vector2, , int, s, 16, 4, 0x4);
  VDUP(vector2, , int, s, 32, 2, 0x2);
  VDUP(vector2, , uint, u, 16, 4, 0x4);
  VDUP(vector2, , uint, u, 32, 2, 0x2);

  /* Choose lane arbitrarily.  */
  TEST_VMULL_LANE(int, s, 16, 32, 4, 2);
  TEST_VMULL_LANE(int, s, 32, 64, 2, 1);
  TEST_VMULL_LANE(uint, u, 16, 32, 4, 2);
  TEST_VMULL_LANE(uint, u, 32, 64, 2, 1);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vmull_lane ();
  return 0;
}

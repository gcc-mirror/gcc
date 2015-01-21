#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,32,4) [] = { 0x11000, 0x11000, 0x11000, 0x11000 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x22000, 0x22000 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0x33000, 0x33000, 0x33000, 0x33000 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0x44000, 0x44000 };

#define INSN_NAME vmull
#define TEST_MSG "VMULL_N"
void exec_vmull_n (void)
{
  int i;

  /* vector_res = vmull_n(vector,val), then store the result.  */
#define TEST_VMULL_N1(INSN, T1, T2, W, W2, N, L)			\
  VECT_VAR(vector_res, T1, W2, N) =					\
    INSN##_n_##T2##W(VECT_VAR(vector, T1, W, N),			\
		     L);						\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

#define TEST_VMULL_N(INSN, T1, T2, W, W2, N, L)	\
  TEST_VMULL_N1(INSN, T1, T2, W, W2, N, L)

  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);

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

  /* Choose multiplier arbitrarily.  */
  TEST_VMULL_N(INSN_NAME, int, s, 16, 32, 4, 0x11);
  TEST_VMULL_N(INSN_NAME, int, s, 32, 64, 2, 0x22);
  TEST_VMULL_N(INSN_NAME, uint, u, 16, 32, 4, 0x33);
  TEST_VMULL_N(INSN_NAME, uint, u, 32, 64, 2, 0x44);

  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
}

int main (void)
{
  exec_vmull_n ();
  return 0;
}

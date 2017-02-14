#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected,int,16,8) [] = { 0x100, 0xe1, 0xc4, 0xa9,
					0x90, 0x79, 0x64, 0x51 };
VECT_VAR_DECL(expected,int,32,4) [] = { 0x100, 0xe1, 0xc4, 0xa9 };
VECT_VAR_DECL(expected,int,64,2) [] = { 0x100, 0xe1 };
VECT_VAR_DECL(expected,uint,16,8) [] = { 0xe100, 0xe2e1, 0xe4c4, 0xe6a9,
					 0xe890, 0xea79, 0xec64, 0xee51 };
VECT_VAR_DECL(expected,uint,32,4) [] = { 0xffe00100, 0xffe200e1,
					 0xffe400c4, 0xffe600a9 };
VECT_VAR_DECL(expected,uint,64,2) [] = { 0xffffffe000000100,
					 0xffffffe2000000e1 };
VECT_VAR_DECL(expected,poly,16,8) [] = { 0x5500, 0x5501, 0x5504, 0x5505,
					 0x5510, 0x5511, 0x5514, 0x5515 };

#define TEST_MSG "VMULL"
void exec_vmull (void)
{
  /* Basic test: y=vmull(x,x), then store the result.  */
#define TEST_VMULL(T1, T2, W, W2, N)					\
  VECT_VAR(vector_res, T1, W2, N) =					\
    vmull_##T2##W(VECT_VAR(vector, T1, W, N),				\
		  VECT_VAR(vector, T1, W, N));				\
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N), VECT_VAR(vector_res, T1, W2, N))

  DECL_VARIABLE(vector, int, 8, 8);
  DECL_VARIABLE(vector, int, 16, 4);
  DECL_VARIABLE(vector, int, 32, 2);
  DECL_VARIABLE(vector, uint, 8, 8);
  DECL_VARIABLE(vector, uint, 16, 4);
  DECL_VARIABLE(vector, uint, 32, 2);
  DECL_VARIABLE(vector, poly, 8, 8);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);
  DECL_VARIABLE(vector_res, poly, 16, 8);

  clean_results ();

  VLOAD(vector, buffer, , int, s, 8, 8);
  VLOAD(vector, buffer, , int, s, 16, 4);
  VLOAD(vector, buffer, , int, s, 32, 2);
  VLOAD(vector, buffer, , uint, u, 8, 8);
  VLOAD(vector, buffer, , uint, u, 16, 4);
  VLOAD(vector, buffer, , uint, u, 32, 2);
  VLOAD(vector, buffer, , poly, p, 8, 8);

  TEST_VMULL(int, s, 8, 16, 8);
  TEST_VMULL(int, s, 16, 32, 4);
  TEST_VMULL(int, s, 32, 64, 2);
  TEST_VMULL(uint, u, 8, 16, 8);
  TEST_VMULL(uint, u, 16, 32, 4);
  TEST_VMULL(uint, u, 32, 64, 2);
  TEST_VMULL(poly, p, 8, 16, 8);

  CHECK(TEST_MSG, int, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, int, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, int, 64, 2, PRIx64, expected, "");
  CHECK(TEST_MSG, uint, 16, 8, PRIx16, expected, "");
  CHECK(TEST_MSG, uint, 32, 4, PRIx32, expected, "");
  CHECK(TEST_MSG, uint, 64, 2, PRIx64, expected, "");
  CHECK_POLY(TEST_MSG, poly, 16, 8, PRIx16, expected, "");
}

int main (void)
{
  exec_vmull ();
  return 0;
}

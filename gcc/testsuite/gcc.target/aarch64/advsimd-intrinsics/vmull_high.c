/* { dg-skip-if "" { arm*-*-* } } */

#include <arm_neon.h>
#include "arm-neon-ref.h"
#include "compute-ref-data.h"

/* Expected results.  */
VECT_VAR_DECL(expected, int, 16, 8) [] = { 0x40, 0x31,0x24, 0x19,
					   0x10, 0x9, 0x4, 0x1 };
VECT_VAR_DECL(expected, int, 32, 4) [] = { 0x90, 0x79, 0x64, 0x51 };
VECT_VAR_DECL(expected, int, 64, 2) [] = { 0xc4, 0xa9 };
VECT_VAR_DECL(expected, uint, 16, 8) [] = { 0xf040, 0xf231, 0xf424, 0xf619,
					    0xf810, 0xfa09, 0xfc04, 0xfe01 };
VECT_VAR_DECL(expected, uint, 32, 4) [] = { 0xffe80090, 0xffea0079,
					    0xffec0064, 0xffee0051 };
VECT_VAR_DECL(expected, uint, 64, 2) [] = { 0xffffffe4000000c4,
					    0xffffffe6000000a9 };
VECT_VAR_DECL(expected, poly, 16, 8) [] = { 0x5540, 0x5541, 0x5544, 0x5545,
					    0x5550, 0x5551, 0x5554, 0x5555 };

#define TEST_MSG "VMULL_HIGH"
void exec_vmull_high (void)
{
  /* Basic test: y = vmull_high(x, x), then store the result.  */
#define TEST_VMULL_HIGH(T1, T2, W1, W2, N1, N2)				 \
  VECT_VAR(vector_res, T1, W2, N1) =					 \
    vmull_high_##T2##W1(VECT_VAR(vector, T1, W1, N2),			 \
			VECT_VAR(vector, T1, W1, N2));			 \
  vst1q_##T2##W2(VECT_VAR(result, T1, W2, N1),				 \
		 VECT_VAR(vector_res, T1, W2, N1))

  DECL_VARIABLE(vector, int, 8, 16);
  DECL_VARIABLE(vector, int, 16, 8);
  DECL_VARIABLE(vector, int, 32, 4);
  DECL_VARIABLE(vector, uint, 8, 16);
  DECL_VARIABLE(vector, uint, 16, 8);
  DECL_VARIABLE(vector, uint, 32, 4);
  DECL_VARIABLE(vector, poly, 8, 16);
  DECL_VARIABLE(vector_res, int, 16, 8);
  DECL_VARIABLE(vector_res, int, 32, 4);
  DECL_VARIABLE(vector_res, int, 64, 2);
  DECL_VARIABLE(vector_res, uint, 16, 8);
  DECL_VARIABLE(vector_res, uint, 32, 4);
  DECL_VARIABLE(vector_res, uint, 64, 2);
  DECL_VARIABLE(vector_res, poly, 16, 8);

  clean_results ();

  VLOAD(vector, buffer, q, int, s, 8, 16);
  VLOAD(vector, buffer, q, int, s, 16, 8);
  VLOAD(vector, buffer, q, int, s, 32, 4);
  VLOAD(vector, buffer, q, uint, u, 8, 16);
  VLOAD(vector, buffer, q, uint, u, 16, 8);
  VLOAD(vector, buffer, q, uint, u, 32, 4);
  VLOAD(vector, buffer, q, poly, p, 8, 16);

  TEST_VMULL_HIGH(int, s, 8, 16, 8, 16);
  TEST_VMULL_HIGH(int, s, 16, 32, 4, 8);
  TEST_VMULL_HIGH(int, s, 32, 64, 2, 4);
  TEST_VMULL_HIGH(uint, u, 8, 16, 8, 16);
  TEST_VMULL_HIGH(uint, u, 16, 32, 4, 8);
  TEST_VMULL_HIGH(uint, u, 32, 64, 2, 4);
  TEST_VMULL_HIGH(poly, p, 8, 16, 8, 16);

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
  exec_vmull_high ();
  return 0;
}

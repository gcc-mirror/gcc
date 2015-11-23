/* Test the vmulxs_f32 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"
#include "vmulx.x"

extern void abort (void);

int
main (void)
{
  float32_t v1 = 3.14159265359;
  float32_t v2 = 1.383894;

  /* Constant * constant, shouldn't generete fmulx or fmul, only fmov.  */
  SETUP_TEST_CASE_SCALAR (1, vmulxs_f32, float32_t, v1, v2, v1 * v2);
  SETUP_TEST_CASE_SCALAR (2, vmulxs_f32, float32_t, 0.0,
			  __builtin_huge_valf (), 2.0);
  SETUP_TEST_CASE_SCALAR (3, vmulxs_f32, float32_t, 0.0,
			  -__builtin_huge_valf (), -2.0);
  SETUP_TEST_CASE_SCALAR (4, vmulxs_f32, float32_t, -0.0,
			  __builtin_huge_valf (), -2.0);
  SETUP_TEST_CASE_SCALAR (5, vmulxs_f32, float32_t, -0.0,
			  -__builtin_huge_valf (), 2.0);
  /* Constant +/- 0 or +/- inf * non-constant should generate fmulx.  */
  SETUP_TEST_CASE_SCALAR (6, vmulxs_f32, float32_t, foo32 (),
			  -__builtin_huge_valf (), -__builtin_huge_valf ());
  SETUP_TEST_CASE_SCALAR (7, vmulxs_f32, float32_t, foo32 (),
			  __builtin_huge_valf (), __builtin_huge_valf ());
  SETUP_TEST_CASE_SCALAR (8, vmulxs_f32, float32_t, foo32 (),
			  0, 0);
  SETUP_TEST_CASE_SCALAR (9, vmulxs_f32, float32_t, foo32 (),
			  -0.0, -0.0);
  /* Constant non +/- 0 or non +/- inf * non-constant should generate fmul.  */
  SETUP_TEST_CASE_SCALAR (10, vmulxs_f32, float32_t, foo32 (),
			  v1, v1);
  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+\n" 4 } } */
/* { dg-final { scan-assembler-times "fmul\[ \t\]+\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+, ?\[sS\]\[0-9\]+\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[sS\]\[0-9\]+, ?2.0e\\+0\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[sS\]\[0-9\]+, ?-2.0e\\+0\n" 1 } } */

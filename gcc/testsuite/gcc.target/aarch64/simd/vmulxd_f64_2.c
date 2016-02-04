/* Test the vmulxd_f64 AArch64 SIMD intrinsic.  */

/* { dg-do run } */
/* { dg-options "-save-temps -O3" } */

#include "arm_neon.h"
#include "vmulx.x"

extern void abort (void);

int
main (void)
{
  float64_t v1 = 3.14159265359;
  float64_t v2 = 1.383894;

  /* Constant * constant, shouldn't generete fmulx or fmul, only fmov.  */
  SETUP_TEST_CASE_SCALAR (1, vmulxd_f64, float64_t, v1, v2, v1 * v2);
  SETUP_TEST_CASE_SCALAR (2, vmulxd_f64, float64_t, 0.0,
			  __builtin_huge_val (), 2.0);
  SETUP_TEST_CASE_SCALAR (3, vmulxd_f64, float64_t, 0.0,
			  -__builtin_huge_val (), -2.0);
  SETUP_TEST_CASE_SCALAR (4, vmulxd_f64, float64_t, -0.0,
			  __builtin_huge_val (), -2.0);
  SETUP_TEST_CASE_SCALAR (5, vmulxd_f64, float64_t, -0.0,
			  -__builtin_huge_val (), 2.0);
  /* Constant +/- 0 or +/- inf * non-constant should generate fmulx.  */
  SETUP_TEST_CASE_SCALAR (6, vmulxd_f64, float64_t, foo64 (),
			  -__builtin_huge_val (), -__builtin_huge_val ());
  SETUP_TEST_CASE_SCALAR (7, vmulxd_f64, float64_t, foo64 (),
			  __builtin_huge_val (), __builtin_huge_val ());
  SETUP_TEST_CASE_SCALAR (8, vmulxd_f64, float64_t, foo64 (),
			  0, 0);
  SETUP_TEST_CASE_SCALAR (9, vmulxd_f64, float64_t, foo64 (),
			  -0.0, -0.0);
  /* Constant non +/- 0 or non +/- inf * non-constant should generate fmul.  */
  SETUP_TEST_CASE_SCALAR (10, vmulxd_f64, float64_t, foo64 (),
			  v1, v1);

  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 4 } } */
/* { dg-final { scan-assembler-times "fmul\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[dD\]\[0-9\]+, ?2.0e\\+0\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[dD\]\[0-9\]+, ?-2.0e\\+0\n" 1 } } */

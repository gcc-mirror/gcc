/* Test the vmulx_f64 AArch64 SIMD intrinsic.  */

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
  SETUP_TEST_CASE_VEC (1, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (v1), PASS_ARRAY (v2), PASS_ARRAY (v1 * v2),
		       1, f64, , , ,);
  SETUP_TEST_CASE_VEC (2, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (0.0), PASS_ARRAY (__builtin_huge_val ()),
		       PASS_ARRAY (2.0), 1, f64, , , ,);
  SETUP_TEST_CASE_VEC (3, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (0.0), PASS_ARRAY (-__builtin_huge_val ()),
		       PASS_ARRAY (-2.0), 1, f64, , , ,);
  SETUP_TEST_CASE_VEC (4, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (-0.0), PASS_ARRAY (__builtin_huge_val ()),
		       PASS_ARRAY (-2.0), 1, f64, , , ,);
  SETUP_TEST_CASE_VEC (5, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (-0.0), PASS_ARRAY (-__builtin_huge_val ()),
		       PASS_ARRAY (2.0), 1, f64, , , ,);
  /* Constant +/- 0 or +/- inf * non-constant should generate fmulx.  */
  SETUP_TEST_CASE_VEC (6, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (/* volatile.  */1.0),
		       PASS_ARRAY (-__builtin_huge_val ()),
		       PASS_ARRAY (-__builtin_huge_val ()), 1, f64, , , volatile
		       ,);
  SETUP_TEST_CASE_VEC (7, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (/* volatile.  */1.0),
		       PASS_ARRAY (__builtin_huge_val ()),
		       PASS_ARRAY (__builtin_huge_val ()), 1, f64, , , volatile
		       ,);
  SETUP_TEST_CASE_VEC (8, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (/* volatile.  */1.0), PASS_ARRAY (0.0),
		       PASS_ARRAY (0.0), 1, f64, , , volatile,);
  SETUP_TEST_CASE_VEC (9, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (/* volatile.  */1.0), PASS_ARRAY (-0.0),
		       PASS_ARRAY (-0.0), 1, f64, , , volatile,);
  /* Constant non +/- 0 or non +/- inf * non-constant should generate fmul.  */
  SETUP_TEST_CASE_VEC (10, vmulx_f64, float64_t, float64x1_t, float64x1_t,
		       PASS_ARRAY (/* volatile.  */1.0), PASS_ARRAY (v1),
		       PASS_ARRAY (v1), 1, f64, , , volatile,);
  return 0;
}
/* { dg-final { scan-assembler-times "fmulx\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 4 } } */
/* { dg-final { scan-assembler-times "fmul\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[dD\]\[0-9\]+, ?2.0e\\+0\n" 1 } } */
/* { dg-final { scan-assembler-times "fmov\[ \t\]+\[dD\]\[0-9\]+, ?-2.0e\\+0\n" 1 } } */

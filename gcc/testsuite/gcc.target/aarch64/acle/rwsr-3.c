/* Test the __arm_[r,w]sr ACLE intrinsics family.  */
/* Ensure that illegal behavior is rejected by the compiler.  */

/* { dg-do compile } */
/* { dg-options "-std=c2x -O3 -march=armv8-a" } */

#include <arm_acle.h>

void
test_non_const_sysreg_name ()
{
  const char *regname = "trcseqstr";
  long long a = __arm_rsr64 (regname); /* { dg-error "first argument to '__builtin_aarch64_rsr64' must be a string literal" } */
  __arm_wsr64 (regname, a); /* { dg-error "first argument to '__builtin_aarch64_wsr64' must be a string literal" } */

  long long b = __arm_rsr64 (nullptr); /* { dg-error "first argument to '__builtin_aarch64_rsr64' must be a string literal" } */
  __arm_wsr64 (nullptr, b); /* { dg-error "first argument to '__builtin_aarch64_wsr64' must be a string literal" } */
}

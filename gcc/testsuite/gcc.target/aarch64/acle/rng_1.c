/* Test the __rndr ACLE intrinsic.  */
/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8.5-a+rng" } */

#include <arm_acle.h>

#ifdef __ARM_FEATURE_RNG
/* Check that instruction is generated when status result is unused.  */
uint64_t
test_rndr_no_stat (void)
{
  uint64_t res;
  __rndr (&res);
  return res;
}

/* Check that instruction is generated when random number result
   is unused.  */
int
test_rndr_error_check (void)
{
  uint64_t res;
  int fail = __rndr (&res);
  if (fail)
    return 0;
  return -1;
}

/* { dg-final { scan-assembler-times "mrs\tx..?, RNDR\n" 2 } } */

/* Check that instruction is generated when status result is unused.  */
uint64_t
test_rndrrs_no_stat (void)
{
  uint64_t res;
  __rndrrs (&res);
  return res;
}

/* Check that instruction is generated when random number result
   is unused.  */
int
test_rndrrs_error_check (void)
{
  uint64_t res;
  int fail = __rndrrs (&res);
  if (fail)
    return 0;
  return -1;
}

/* { dg-final { scan-assembler-times "mrs\tx..?, RNDRRS\n" 2 } } */
#endif

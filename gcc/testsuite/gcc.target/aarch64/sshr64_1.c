/* Test SIMD shift works correctly.  */
/* { dg-do run } */
/* { dg-options "-O3 --save-temps" } */

#include "arm_neon.h"

extern void abort (void);

int __attribute__ ((noinline))
test_sshr64 ()
{
  int64x1_t arg;
  int64x1_t result;
  int64_t got;
  int64_t exp;
  arg = vcreate_s64 (0x0000000080000000);
  result = vshr_n_s64 (arg, 64);
  got = vget_lane_s64 (result, 0);
  exp = 0;
  /* Expect: "result" = 0000000000000000.  */
  if (exp != got)
    return 1;
  return 0;
}

int __attribute__ ((noinline))
test_sshr64_neg ()
{
  int64x1_t arg;
  int64x1_t result;
  int64_t got;
  int64_t exp;
  arg = vcreate_s64 (0xffffffff80000000);
  result = vshr_n_s64 (arg, 64);
  got = vget_lane_s64 (result, 0);
  exp = 0xffffffffffffffff;
  /* Expect: "result" = -1.  */
  if (exp != got)
    return 1;
  return 0;
}

int
__attribute__ ((noinline))
test_other ()
{
  int64x1_t arg;
  int64x1_t result;
  int64_t got;
  int64_t exp;
  arg = vcreate_s64 (0x0000000080000000);
  result = vshr_n_s64 (arg, 4);
  got = vget_lane_s64 (result, 0);
  exp = 0x0000000008000000;
  /* Expect: "result" = 0x0000000008000000.  */
  if (exp != got)
    return 1;
  return 0;
}

int __attribute__ ((noinline))
test_other_neg ()
{
  int64x1_t arg;
  int64x1_t result;
  int64_t got;
  int64_t exp;
  arg = vcreate_s64 (0xffffffff80000000);
  result = vshr_n_s64 (arg, 4);
  got = vget_lane_s64 (result, 0);
  exp = 0xfffffffff8000000;
  /* Expect: "result" = 0xfffffffff8000000.  */
  if (exp != got)
    return 1;
  return 0;
}

int __attribute__ ((noinline))
test_no_sshr0 ()
{
  int64x1_t arg;
  int64x1_t result;
  int64_t got;
  int64_t exp;
  arg = vcreate_s64 (0x0000000080000000);
  result = vshr_n_s64 (arg, 0);
  got = vget_lane_s64 (result, 0);
  exp = 0x0000000080000000;
  /* Expect: "result" = 0x0000000080000000.  */
  if (exp != got)
    return 1;
  return 0;
}

/* { dg-final { scan-assembler-not "sshr\\td\[0-9\]+, d\[0-9\]+, 0" } } */
int
main ()
{
  if (test_sshr64 ())
    abort ();
  if (test_other ())
    abort ();

  if (test_sshr64_neg ())
    abort ();
  if (test_other_neg ())
    abort ();

  if (test_no_sshr0 ())
    abort ();

  return 0;
}


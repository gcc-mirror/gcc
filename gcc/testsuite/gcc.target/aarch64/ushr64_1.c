/* Test logical SIMD shift works correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps" } */

#include "arm_neon.h"

extern void abort (void);

int __attribute__ ((noinline))
test_vshr_n_u64_64 (uint64x1_t passed, uint64_t expected)
{
  return vget_lane_u64 (vshr_n_u64 (passed, 64), 0) != expected;
}

int __attribute__ ((noinline))
test_vshr_n_u64_4 (uint64x1_t passed, uint64_t expected)
{
  return vget_lane_u64 (vshr_n_u64 (passed, 4), 0) != expected;
}

int __attribute__ ((noinline))
test_vshr_n_u64_0 (uint64x1_t passed, uint64_t expected)
{
  return vget_lane_u64 (vshr_n_u64 (passed, 0), 0) != expected;
}

int __attribute__ ((noinline))
test_vshrd_n_u64_64 (uint64_t passed, uint64_t expected)
{
  return vshrd_n_u64 (passed, 64) != expected;
}

int __attribute__ ((noinline))
test_vshrd_n_u64_4 (uint64_t passed, uint64_t expected)
{
  return vshrd_n_u64 (passed, 4) != expected;
}

int __attribute__ ((noinline))
test_vshrd_n_u64_0 (uint64_t passed, uint64_t expected)
{
  return vshrd_n_u64 (passed, 0) != expected;
}

/* { dg-final { (scan-assembler-times "ushr\\td\[0-9\]+, d\[0-9\]+, 4" 2)  || \
   (scan-assembler-times "lsr\\tx\[0-9\]+, x\[0-9\]+, 4" 2) } } */
/* { dg-final { scan-assembler-not "ushr\\td\[0-9\]+, d\[0-9\]+, 0" } } */

int
main (int argc, char *argv[])
{
  /* Testing vshr_n_u64.  */
  if (test_vshr_n_u64_64 (vcreate_u64 (0x0000000080000000), 0))
    abort ();
  if (test_vshr_n_u64_64 (vcreate_u64 (0xffffffff80000000), 0))
    abort ();

  if (test_vshr_n_u64_4 (vcreate_u64 (0x0000000080000000), 0x0000000008000000))
    abort ();
  if (test_vshr_n_u64_4 (vcreate_u64 (0xffffffff80000000), 0x0ffffffff8000000))
    abort ();

  if (test_vshr_n_u64_0 (vcreate_u64 (0x0000000080000000), 0x0000000080000000))
    abort ();

  /* Testing vshrd_n_u64.  */
  if (test_vshrd_n_u64_64 (0x0000000080000000, 0))
    abort ();
  if (test_vshrd_n_u64_64 (0xffffffff80000000, 0))
    abort ();

  if (test_vshrd_n_u64_4 (0x0000000080000000, 0x0000000008000000))
    abort ();
  if (test_vshrd_n_u64_4 (0xffffffff80000000, 0x0ffffffff8000000))
    abort ();

  if (test_vshrd_n_u64_0 (0x0000000080000000, 0x0000000080000000))
    abort ();

  return 0;
}


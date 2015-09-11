/* Test vqneg_s64 intrinsics work correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps" } */

#include <arm_neon.h>

extern void abort (void);

int __attribute__ ((noinline))
test_vqneg_s64 (int64x1_t passed, int64_t expected)
{
  return vget_lane_s64 (vqneg_s64 (passed), 0) != expected;
}

int __attribute__ ((noinline))
test_vqnegd_s64 (int64_t passed, int64_t expected)
{
  return vqnegd_s64 (passed) != expected;
}

/* { dg-final { scan-assembler-times "sqneg\\td\[0-9\]+, d\[0-9\]+" 2 } } */

int
main (int argc, char **argv)
{
  /* Basic test.  */
  if (test_vqneg_s64 (vcreate_s64 (-1), 1))
    abort ();
  if (test_vqnegd_s64 (-1, 1))
    abort ();

  /* Negating max int64_t.  */
  if (test_vqneg_s64 (vcreate_s64 (0x7fffffffffffffff), 0x8000000000000001))
    abort ();
  if (test_vqnegd_s64 (0x7fffffffffffffff, 0x8000000000000001))
    abort ();

  /* Negating min int64_t.
     Note, exact negation cannot be represented as int64_t.  */
  if (test_vqneg_s64 (vcreate_s64 (0x8000000000000000), 0x7fffffffffffffff))
    abort ();
  if (test_vqnegd_s64 (0x8000000000000000, 0x7fffffffffffffff))
    abort ();

  return 0;
}

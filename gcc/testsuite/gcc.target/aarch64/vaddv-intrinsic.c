
/* { dg-do run } */
/* { dg-options "-O3" } */

#include "arm_neon.h"

extern void abort (void);

#include "vaddv-intrinsic.x"

int
main (void)
{
  const float32_t pool_v2sf[] = {4.0f, 9.0f};
  const float32_t pool_v4sf[] = {4.0f, 9.0f, 16.0f, 25.0f};
  const float64_t pool_v2df[] = {4.0, 9.0};

  if (test_vaddv_v2sf (pool_v2sf) != 13.0f)
    abort ();

  if (test_vaddv_v4sf (pool_v4sf) != 54.0f)
    abort ();

  if (test_vaddv_v2df (pool_v2df) != 13.0)
    abort ();

  return 0;
}

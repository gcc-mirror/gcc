/* { dg-do assemble } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

uint32_t
foo (uint64_t v0)
{
  return vqshrnd_n_u64 (vshrd_n_u64 (v0, 26), 7);
}


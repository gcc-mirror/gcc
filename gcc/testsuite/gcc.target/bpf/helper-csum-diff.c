/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int64_t ret;
  int32_t *to, *from;
  uint64_t to_size, from_size;
  int seed;

  ret = __builtin_bpf_helper_csum_diff (from, from_size, to, to_size, seed);
}

/* { dg-final { scan-assembler "call\t28" } } */

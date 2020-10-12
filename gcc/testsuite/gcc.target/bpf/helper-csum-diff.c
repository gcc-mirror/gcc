/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int64_t ret;
  int32_t *to, *from;
  uint64_t to_size, from_size;
  int seed;

  ret = bpf_csum_diff (from, from_size, to, to_size, seed);
}

/* { dg-final { scan-assembler "call\t28" } } */

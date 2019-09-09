/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ctx, *prog_array_map;
  uint32_t index;

  __builtin_bpf_helper_tail_call (ctx, prog_array_map, index);
}

/* { dg-final { scan-assembler "call\t12" } } */

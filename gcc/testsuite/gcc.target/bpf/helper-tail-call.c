/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *prog_array_map;
  uint32_t index;

  ret = bpf_tail_call (ctx, prog_array_map, index);
}

/* { dg-final { scan-assembler "call\t12" } } */

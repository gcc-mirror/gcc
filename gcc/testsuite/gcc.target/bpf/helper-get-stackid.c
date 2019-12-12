/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx, *map;
  uint64_t flags;

  ret = __builtin_bpf_helper_get_stackid (ctx, map, flags);
}

/* { dg-final { scan-assembler "call\t27" } } */

/* { dg-do compile } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *map;
  uint64_t flags;
  void *data;
  uint64_t size;

  ret = bpf_perf_event_output (ctx, map, flags, data, size);
}

/* { dg-final { scan-assembler "call\t25" } } */

/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint64_t ret;
  void *map;
  uint64_t flags;

  ret = __builtin_bpf_helper_perf_event_read (map, flags);
}

/* { dg-final { scan-assembler "call\t22" } } */

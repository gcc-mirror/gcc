/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint64_t ret;
  void *map;
  uint64_t flags;

  ret = bpf_perf_event_read (map, flags);
}

/* { dg-final { scan-assembler "call\t22" } } */

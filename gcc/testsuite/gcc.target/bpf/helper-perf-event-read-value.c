/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *map, *buf;
  uint64_t flags;
  uint64_t buf_size;

  ret = bpf_perf_event_read_value (map, flags, buf, buf_size);
}

/* { dg-final { scan-assembler "call\t55" } } */

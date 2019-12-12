/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *map, *buf;
  uint64_t flags;
  uint64_t buf_size;
  
  ret = __builtin_bpf_helper_perf_event_read_value (map, flags, buf, buf_size);
}

/* { dg-final { scan-assembler "call\t55" } } */

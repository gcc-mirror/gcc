/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx, *buf;
  uint64_t buf_size;
  
  ret = __builtin_bpf_helper_perf_prog_read_value (ctx, buf, buf_size);
}

/* { dg-final { scan-assembler "call\t56" } } */

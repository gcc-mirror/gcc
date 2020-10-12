/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *buf;
  uint64_t buf_size;

  ret = bpf_perf_prog_read_value (ctx, buf, buf_size);
}

/* { dg-final { scan-assembler "call\t56" } } */

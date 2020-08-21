/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <stddef.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *buf;
  size_t buf_len;
  uint64_t flags;

  ret = bpf_sysctl_get_name (ctx, buf, buf_len, flags);
}

/* { dg-final { scan-assembler "call\t101" } } */

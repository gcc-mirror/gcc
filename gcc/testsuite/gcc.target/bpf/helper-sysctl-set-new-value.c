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

  ret = bpf_sysctl_set_new_value (ctx, buf, buf_len);
}

/* { dg-final { scan-assembler "call\t104" } } */

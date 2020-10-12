/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *dst, *src;
  uint32_t len;

  ret = bpf_probe_write_user (dst, src, len);
}

/* { dg-final { scan-assembler "call\t36" } } */

/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *dst, *src;
  uint32_t len;

  ret = __builtin_bpf_helper_probe_write_user (dst, src, len);
}

/* { dg-final { scan-assembler "call\t36" } } */

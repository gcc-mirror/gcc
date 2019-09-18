/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *src, *dst;
  uint32_t size;

  ret = __builtin_bpf_helper_probe_read (dst, size, src);
}

/* { dg-final { scan-assembler "call\t4" } } */

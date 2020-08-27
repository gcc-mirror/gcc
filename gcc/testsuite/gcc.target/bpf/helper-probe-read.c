/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *src, *dst;
  uint32_t size;

  ret = bpf_probe_read (dst, size, src);
}

/* { dg-final { scan-assembler "call\t4" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <stddef.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *buf;
  unsigned long res;
  uint64_t flags;
  size_t buf_len;

  ret = bpf_strtoul (buf, buf_len, flags, &res);
}

/* { dg-final { scan-assembler "call\t106" } } */

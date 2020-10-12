/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *addr;
  int addr_len;

  ret = bpf_bind (ctx, addr, addr_len);
}

/* { dg-final { scan-assembler "call\t64" } } */

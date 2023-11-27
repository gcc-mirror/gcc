/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>

int bpf_bind (void *ctx, void *addr, int addr_len)
  __attribute__((kernel_helper(64)));

void
foo ()
{
  int ret;
  void *ctx, *addr;
  int addr_len;

  ret = bpf_bind (ctx, addr, addr_len);
}

/* { dg-final { scan-assembler "call\t64" } } */

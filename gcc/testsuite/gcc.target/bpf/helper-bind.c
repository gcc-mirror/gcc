/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx, *addr;
  int addr_len;

  ret = __builtin_bpf_helper_bind (ctx, addr, addr_len);
}

/* { dg-final { scan-assembler "call\t64" } } */

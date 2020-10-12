/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx, *params;
  int plen;
  uint32_t flags;

  ret = bpf_fib_lookup (ctx, params, plen, flags);
}

/* { dg-final { scan-assembler "call\t69" } } */

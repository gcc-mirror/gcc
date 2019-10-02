/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx, *params;
  int plen;
  uint32_t flags;

  ret = __builtin_bpf_helper_fib_lookup (ctx, params, plen, flags);
}

/* { dg-final { scan-assembler "call\t69" } } */

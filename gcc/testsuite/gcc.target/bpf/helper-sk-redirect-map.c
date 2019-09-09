/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx, *map;
  uint32_t key;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_sk_redirect_map (ctx, map, key, flags);
}

/* { dg-final { scan-assembler "call\t52" } } */

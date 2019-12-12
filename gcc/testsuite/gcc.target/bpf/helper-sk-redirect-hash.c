/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *map, *key;
  uint64_t flags;

  ret = __builtin_bpf_helper_sk_redirect_hash (skb, map, key,
					       flags);
}

/* { dg-final { scan-assembler "call\t72" } } */

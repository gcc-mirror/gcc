/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *map, *key;
  uint64_t flags;

  ret = bpf_sk_redirect_hash (skb, map, key, flags);
}

/* { dg-final { scan-assembler "call\t72" } } */

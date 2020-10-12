/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *key;
  uint32_t size;
  uint64_t flags;

  ret = bpf_skb_set_tunnel_key (skb, key, size, flags);
}

/* { dg-final { scan-assembler "call\t21" } } */

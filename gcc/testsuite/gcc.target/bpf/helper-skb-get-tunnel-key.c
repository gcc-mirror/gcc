/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *key;
  uint32_t size;
  uint64_t flags;

  ret = __builtin_bpf_helper_skb_get_tunnel_key (skb, key, size, flags);
}

/* { dg-final { scan-assembler "call\t20" } } */

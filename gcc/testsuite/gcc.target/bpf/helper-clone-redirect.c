/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t ifindex;
  uint64_t flags;

  ret = __builtin_bpf_helper_clone_redirect (skb, ifindex, flags);
}

/* { dg-final { scan-assembler "call\t13" } } */

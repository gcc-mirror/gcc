/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  int16_t proto;
  uint64_t flags;

  ret = __builtin_bpf_helper_skb_change_proto (skb, proto, flags);
}

/* { dg-final { scan-assembler "call\t31" } } */

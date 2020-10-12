/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  int16_t proto;
  uint64_t flags;

  ret = bpf_skb_change_proto (skb, proto, flags);
}

/* { dg-final { scan-assembler "call\t31" } } */

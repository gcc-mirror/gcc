/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;

  ret = __builtin_bpf_helper_skb_vlan_pop (skb);
}

/* { dg-final { scan-assembler "call\t19" } } */

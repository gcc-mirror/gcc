/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  int16_t vlan_proto;
  uint16_t vlan_tci;

  ret = __builtin_bpf_helper_skb_vlan_push (skb, vlan_proto, vlan_tci);
}

/* { dg-final { scan-assembler "call\t18" } } */

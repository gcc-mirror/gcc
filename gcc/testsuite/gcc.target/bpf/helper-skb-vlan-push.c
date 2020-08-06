/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  int16_t vlan_proto;
  uint16_t vlan_tci;

  ret = bpf_skb_vlan_push (skb, vlan_proto, vlan_tci);
}

/* { dg-final { scan-assembler "call\t18" } } */

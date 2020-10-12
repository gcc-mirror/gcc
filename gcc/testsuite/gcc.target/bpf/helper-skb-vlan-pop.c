/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;

  ret = bpf_skb_vlan_pop (skb);
}

/* { dg-final { scan-assembler "call\t19" } } */

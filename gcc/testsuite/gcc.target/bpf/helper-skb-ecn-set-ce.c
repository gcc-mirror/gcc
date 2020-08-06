/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;

  ret = bpf_skb_ecn_set_ce (skb);
}

/* { dg-final { scan-assembler "call\t97" } } */

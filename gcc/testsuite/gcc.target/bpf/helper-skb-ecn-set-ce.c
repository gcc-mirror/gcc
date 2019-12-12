/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;

  ret = __builtin_bpf_helper_skb_ecn_set_ce (skb);
}

/* { dg-final { scan-assembler "call\t97" } } */

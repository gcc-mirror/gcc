/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t type;

  ret = __builtin_bpf_helper_skb_change_type (skb, type);
}

/* { dg-final { scan-assembler "call\t32" } } */

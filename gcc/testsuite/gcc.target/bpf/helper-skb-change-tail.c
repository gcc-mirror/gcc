/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t len;
  uint64_t flags;

  ret = __builtin_bpf_helper_skb_change_tail (skb, len, flags);
}

/* { dg-final { scan-assembler "call\t38" } } */

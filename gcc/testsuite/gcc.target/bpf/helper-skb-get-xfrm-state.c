/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *xfrm_state;
  uint32_t index, size;
  uint64_t flags;

  ret = __builtin_bpf_helper_skb_get_xfrm_state (skb, index,
						 xfrm_state, size, flags);
}

/* { dg-final { scan-assembler "call\t66" } } */

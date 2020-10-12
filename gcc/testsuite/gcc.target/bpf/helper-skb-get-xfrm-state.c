/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *xfrm_state;
  uint32_t index, size;
  uint64_t flags;

  ret = bpf_skb_get_xfrm_state (skb, index,
				xfrm_state, size, flags);
}

/* { dg-final { scan-assembler "call\t66" } } */

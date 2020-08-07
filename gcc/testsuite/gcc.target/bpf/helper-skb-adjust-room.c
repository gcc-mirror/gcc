/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  int32_t len_diff;
  uint32_t mode;
  uint64_t flags;

  ret = bpf_skb_adjust_room (skb, len_diff, mode, flags);
}

/* { dg-final { scan-assembler "call\t50" } } */

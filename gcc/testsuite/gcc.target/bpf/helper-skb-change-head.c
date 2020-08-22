/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t len;
  uint64_t flags;

  ret = bpf_skb_change_head (skb, len, flags);
}

/* { dg-final { scan-assembler "call\t43" } } */

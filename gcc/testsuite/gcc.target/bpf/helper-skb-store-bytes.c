/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset;
  void *from;
  uint32_t len;
  uint64_t flags;

  ret = bpf_skb_store_bytes (skb, offset, from, len, flags);
}

/* { dg-final { scan-assembler "call\t9" } } */

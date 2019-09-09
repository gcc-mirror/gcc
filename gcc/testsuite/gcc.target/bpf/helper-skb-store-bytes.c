/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset;
  void *from;
  uint32_t len;
  uint64_t flags;

  ret = __builtin_bpf_helper_skb_store_bytes (skb, offset, from, len, flags);
}

/* { dg-final { scan-assembler "call\t9" } } */

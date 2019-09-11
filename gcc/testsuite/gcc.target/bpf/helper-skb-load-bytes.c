/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *to;
  uint32_t offset, len;

  ret = __builtin_bpf_helper_skb_load_bytes (skb, offset, to, len);
}

/* { dg-final { scan-assembler "call\t26" } } */

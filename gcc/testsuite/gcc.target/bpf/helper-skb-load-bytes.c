/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *to;
  uint32_t offset, len;

  ret = bpf_skb_load_bytes (skb, offset, to, len);
}

/* { dg-final { scan-assembler "call\t26" } } */

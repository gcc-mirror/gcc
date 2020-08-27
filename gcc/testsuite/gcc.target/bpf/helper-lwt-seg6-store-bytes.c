/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *from;
  uint32_t offset, len;

  ret = bpf_lwt_seg6_store_bytes (skb, offset, from, len);
}

/* { dg-final { scan-assembler "call\t74" } } */

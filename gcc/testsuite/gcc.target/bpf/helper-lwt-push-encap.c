/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *hdr;
  uint32_t type, len;

  ret = bpf_lwt_push_encap (skb, type, hdr, len);
}

/* { dg-final { scan-assembler "call\t73" } } */

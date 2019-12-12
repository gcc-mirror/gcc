/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *hdr;
  uint32_t type, len;
  
  ret = __builtin_bpf_helper_lwt_push_encap (skb, type, hdr, len);
}

/* { dg-final { scan-assembler "call\t73" } } */

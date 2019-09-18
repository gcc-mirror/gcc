/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *from;
  uint32_t offset, len;
  
  ret = __builtin_bpf_helper_lwt_seg6_store_bytes (skb, offset,
						   from, len);
}

/* { dg-final { scan-assembler "call\t74" } } */

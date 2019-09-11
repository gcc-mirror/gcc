/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset, delta;
  
  ret = __builtin_bpf_helper_lwt_seg6_adjust_srh (skb, offset,
						  delta);
}

/* { dg-final { scan-assembler "call\t75" } } */

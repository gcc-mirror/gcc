/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset, delta;

  ret = bpf_lwt_seg6_adjust_srh (skb, offset, delta);
}

/* { dg-final { scan-assembler "call\t75" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint8_t *opt;
  uint32_t size;

  ret = bpf_skb_set_tunnel_opt (skb, opt, size);
}

/* { dg-final { scan-assembler "call\t30" } } */

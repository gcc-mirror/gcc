/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint8_t *opt;
  uint32_t size;

  ret = __builtin_bpf_helper_skb_set_tunnel_opt (skb, opt, size);
}

/* { dg-final { scan-assembler "call\t30" } } */

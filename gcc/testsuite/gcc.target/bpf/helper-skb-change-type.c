/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t type;

  ret = bpf_skb_change_type (skb, type);
}

/* { dg-final { scan-assembler "call\t32" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t len;

  ret = bpf_skb_pull_data (skb, len);
}

/* { dg-final { scan-assembler "call\t39" } } */

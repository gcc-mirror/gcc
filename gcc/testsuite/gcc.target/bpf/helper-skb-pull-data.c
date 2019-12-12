/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t len;

  ret = __builtin_bpf_helper_skb_pull_data (skb, len);
}

/* { dg-final { scan-assembler "call\t39" } } */

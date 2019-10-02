/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int64_t ret;
  void *skb;
  int csum;

  ret = __builtin_bpf_helper_csum_update (skb, csum);
}

/* { dg-final { scan-assembler "call\t40" } } */

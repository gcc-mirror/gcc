/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int64_t ret;
  void *skb;
  int csum;

  ret = bpf_csum_update (skb, csum);
}

/* { dg-final { scan-assembler "call\t40" } } */

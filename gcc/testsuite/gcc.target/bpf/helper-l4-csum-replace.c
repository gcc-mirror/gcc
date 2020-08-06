/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset;
  uint64_t from, to, size;

  ret = bpf_l4_csum_replace (skb, offset, from, to, size);
}

/* { dg-final { scan-assembler "call\t11" } } */

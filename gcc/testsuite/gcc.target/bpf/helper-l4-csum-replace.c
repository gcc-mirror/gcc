/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t offset;
  uint64_t from, to, size;

  ret = __builtin_bpf_helper_l4_csum_replace (skb, offset, from, to, size);
}

/* { dg-final { scan-assembler "call\t11" } } */

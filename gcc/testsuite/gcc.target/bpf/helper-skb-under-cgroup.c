/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *map;
  uint32_t index;

  ret = bpf_skb_under_cgroup (skb, map, index);
}

/* { dg-final { scan-assembler "call\t33" } } */

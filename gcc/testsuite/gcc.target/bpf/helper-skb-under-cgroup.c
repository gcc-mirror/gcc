/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *map;
  uint32_t index;

  ret = __builtin_bpf_helper_skb_under_cgroup (skb, map, index);
}

/* { dg-final { scan-assembler "call\t33" } } */

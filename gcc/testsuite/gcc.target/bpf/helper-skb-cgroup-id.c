/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint64_t ret;
  void *skb;

  ret = bpf_skb_cgroup_id (skb);
}

/* { dg-final { scan-assembler "call\t79" } } */

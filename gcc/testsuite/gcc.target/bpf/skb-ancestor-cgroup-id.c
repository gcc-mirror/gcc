/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  int ancestor_level;

  ret = bpf_skb_ancestor_cgroup_id (skb, ancestor_level);
}

/* { dg-final { scan-assembler "call\t83" } } */

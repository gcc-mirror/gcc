/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  int ancestor_level;
  
  ret = __builtin_bpf_helper_skb_ancestor_cgroup_id (skb,
						     ancestor_level);
}

/* { dg-final { scan-assembler "call\t83" } } */

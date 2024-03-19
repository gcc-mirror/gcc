/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2" } */

#include <stdint.h>

struct __sk_buff;
static uint64_t (*bpf_skb_ancestor_cgroup_id)(struct __sk_buff *skb, int ancestor_level) = (void *) 83;

void
foo ()
{
  int ret;
  void *skb;
  int ancestor_level;

  ret = bpf_skb_ancestor_cgroup_id (skb, ancestor_level);
}

/* { dg-final { scan-assembler "call\t83" } } */

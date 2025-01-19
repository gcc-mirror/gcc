/* { dg-do compile } */
/* { dg-options "-std=gnu99 -O2 -ffreestanding" } */

#include <stdint.h>

struct __sk_buff;
static uint64_t (*bpf_skb_ancestor_cgroup_id)(struct __sk_buff *skb, int ancestor_level) = (void *) 83;
static uint64_t (* const const_bpf_skb_ancestor_cgroup_id)(struct __sk_buff *skb, int ancestor_level) = (void *) 84;

void
foo ()
{
  int ret;
  void *skb;
  int ancestor_level;

  ret = bpf_skb_ancestor_cgroup_id (skb, ancestor_level)
    + const_bpf_skb_ancestor_cgroup_id (skb, ancestor_level);
}

/* { dg-final { scan-assembler "call\t83" } } */
/* { dg-final { scan-assembler "call\t84" } } */

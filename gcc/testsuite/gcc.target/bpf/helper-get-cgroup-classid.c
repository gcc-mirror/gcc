/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint32_t ret;
  void *skb;

  ret = __builtin_bpf_helper_get_cgroup_classid (skb);
}

/* { dg-final { scan-assembler "call\t17" } } */

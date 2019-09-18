/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint64_t ret;
  
  ret = __builtin_bpf_helper_get_current_cgroup_id ();
}

/* { dg-final { scan-assembler "call\t80" } } */

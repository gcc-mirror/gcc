/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *map;
  uint32_t index;

  ret = __builtin_bpf_helper_current_task_under_cgroup (map, index);
}

/* { dg-final { scan-assembler "call\t37" } } */

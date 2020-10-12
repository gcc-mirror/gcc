/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *map;
  uint32_t index;

  ret = bpf_current_task_under_cgroup (map, index);
}

/* { dg-final { scan-assembler "call\t37" } } */

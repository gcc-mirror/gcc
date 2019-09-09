/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;

  ret = __builtin_bpf_helper_get_numa_node_id ();
}

/* { dg-final { scan-assembler "call\t42" } } */

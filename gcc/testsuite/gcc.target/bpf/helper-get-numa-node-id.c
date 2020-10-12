/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;

  ret = bpf_get_numa_node_id ();
}

/* { dg-final { scan-assembler "call\t42" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint64_t ret;

  ret = bpf_get_current_task ();
}

/* { dg-final { scan-assembler "call\t35" } } */

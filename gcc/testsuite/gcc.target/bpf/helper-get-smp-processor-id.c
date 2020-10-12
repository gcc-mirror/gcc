/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint32_t ret;

  ret = bpf_get_smp_processor_id ();
}

/* { dg-final { scan-assembler "call\t8" } } */

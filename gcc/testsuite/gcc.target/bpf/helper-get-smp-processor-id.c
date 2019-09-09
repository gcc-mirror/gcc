/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint32_t ret;

  ret = __builtin_bpf_helper_get_smp_processor_id ();
}

/* { dg-final { scan-assembler "call\t8" } } */

/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint32_t ret;

  ret = __builtin_bpf_helper_get_prandom_u32 ();
}

/* { dg-final { scan-assembler "call\t7" } } */

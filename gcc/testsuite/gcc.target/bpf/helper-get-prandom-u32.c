/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint32_t ret;

  ret = bpf_get_prandom_u32 ();
}

/* { dg-final { scan-assembler "call\t7" } } */

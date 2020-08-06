/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint64_t ret;
  ret = bpf_ktime_get_ns ();
}

/* { dg-final { scan-assembler "call\t5" } } */

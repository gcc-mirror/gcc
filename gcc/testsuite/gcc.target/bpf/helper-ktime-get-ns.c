/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint64_t ret;
  ret = __builtin_bpf_helper_ktime_get_ns ();
}

/* { dg-final { scan-assembler "call\t5" } } */

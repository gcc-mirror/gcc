/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint64_t ret;

  ret = __builtin_bpf_helper_get_current_pid_tgid ();
}

/* { dg-final { scan-assembler "call\t14" } } */

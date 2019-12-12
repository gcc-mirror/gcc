/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *lock;

  __builtin_bpf_helper_spin_lock (lock);
}

/* { dg-final { scan-assembler "call\t93" } } */

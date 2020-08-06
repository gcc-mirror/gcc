/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *lock;

  bpf_spin_unlock (lock);
}

/* { dg-final { scan-assembler "call\t94" } } */

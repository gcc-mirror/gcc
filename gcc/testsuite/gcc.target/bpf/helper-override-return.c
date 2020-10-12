/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *regs;
  uint64_t rc;

  ret = bpf_override_return (regs, rc);
}

/* { dg-final { scan-assembler "call\t58" } } */

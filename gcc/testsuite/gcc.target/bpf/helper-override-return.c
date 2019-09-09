/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *regs;
  uint64_t rc;
  
  ret = __builtin_bpf_helper_override_return (regs, rc);
}

/* { dg-final { scan-assembler "call\t58" } } */

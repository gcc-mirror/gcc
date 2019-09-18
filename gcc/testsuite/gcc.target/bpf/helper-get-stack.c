/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *regs, *buf;
  uint32_t size;
  uint64_t flags;

  ret = __builtin_bpf_helper_get_stack (regs, buf, size, flags);
}

/* { dg-final { scan-assembler "call\t67" } } */

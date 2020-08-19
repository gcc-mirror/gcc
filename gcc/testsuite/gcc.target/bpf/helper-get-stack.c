/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *regs, *buf;
  uint32_t size;
  uint64_t flags;

  ret = bpf_get_stack (regs, buf, size, flags);
}

/* { dg-final { scan-assembler "call\t67" } } */

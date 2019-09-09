/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  uint32_t ifindex;
  uint64_t flags;

  ret = __builtin_bpf_helper_redirect (ifindex, flags);
}

/* { dg-final { scan-assembler "call\t23" } } */

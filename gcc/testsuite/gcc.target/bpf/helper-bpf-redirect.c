/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  uint32_t ifindex;
  uint64_t flags;

  ret = bpf_redirect (ifindex, flags);
}

/* { dg-final { scan-assembler "call\t23" } } */

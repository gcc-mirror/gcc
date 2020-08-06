/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t start, pop;
  uint64_t flags;

  ret = bpf_msg_pop_data (skb, start, pop, flags);
}

/* { dg-final { scan-assembler "call\t91" } } */

/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t start, pop;
  uint64_t flags;

  ret = __builtin_bpf_helper_msg_pop_data (skb, start, pop, flags);
}

/* { dg-final { scan-assembler "call\t91" } } */

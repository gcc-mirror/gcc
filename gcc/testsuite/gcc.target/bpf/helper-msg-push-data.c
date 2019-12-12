/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb;
  uint32_t start, len;
  uint64_t flags;

  ret = __builtin_bpf_helper_msg_push_data (skb, start, len, flags);
}

/* { dg-final { scan-assembler "call\t90" } } */

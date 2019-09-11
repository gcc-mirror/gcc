/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *msg;
  uint32_t start, end;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_msg_pull_data (msg, start, end, flags);
}

/* { dg-final { scan-assembler "call\t63" } } */

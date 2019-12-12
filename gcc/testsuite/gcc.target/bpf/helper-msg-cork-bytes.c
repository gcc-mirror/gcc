/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *msg;
  uint32_t bytes;
  
  ret = __builtin_bpf_helper_msg_cork_bytes (msg, bytes);
}

/* { dg-final { scan-assembler "call\t62" } } */

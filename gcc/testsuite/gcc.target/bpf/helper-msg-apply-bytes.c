/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *msg;
  uint32_t bytes;

  ret = bpf_msg_apply_bytes (msg, bytes);
}

/* { dg-final { scan-assembler "call\t61" } } */

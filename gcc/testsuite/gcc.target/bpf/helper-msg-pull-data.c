/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *msg;
  int len;

  ret = bpf_msg_pull_data (msg, len);
}

/* { dg-final { scan-assembler "call\t63" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *bpf_sock;
  int argval;

  ret = bpf_sock_ops_cb_flags_set (bpf_sock, argval);
}

/* { dg-final { scan-assembler "call\t59" } } */

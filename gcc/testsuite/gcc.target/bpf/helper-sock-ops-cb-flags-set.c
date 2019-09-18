/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *bpf_sock;
  int argval;
  
  ret = __builtin_bpf_helper_sock_ops_cb_flags_set (bpf_sock,
						    argval);
}

/* { dg-final { scan-assembler "call\t59" } } */

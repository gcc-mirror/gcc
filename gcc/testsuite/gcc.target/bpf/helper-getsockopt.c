/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *bpf_socket;
  int level, optname, optlen;
  char *optval;
  
  ret = __builtin_bpf_helper_getsockopt (bpf_socket, level,
					 optname, optval, optlen);
}

/* { dg-final { scan-assembler "call\t57" } } */

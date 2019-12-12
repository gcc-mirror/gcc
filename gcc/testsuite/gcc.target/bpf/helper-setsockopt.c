/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *bpf_socket;
  int level;
  int optname;
  void *optval;
  int optlen;
  
  ret = __builtin_bpf_helper_setsockopt (bpf_socket, level, optname,
					 optval, optlen);
}

/* { dg-final { scan-assembler "call\t49" } } */

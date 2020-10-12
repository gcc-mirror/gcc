/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *bpf_socket;
  int level, optname, optlen;
  char *optval;

  ret = bpf_getsockopt (bpf_socket, level,
			optname, optval, optlen);
}

/* { dg-final { scan-assembler "call\t57" } } */

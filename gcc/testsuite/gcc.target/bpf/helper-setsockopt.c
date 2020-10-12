/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *bpf_socket;
  int level;
  int optname;
  void *optval;
  int optlen;

  ret = bpf_setsockopt (bpf_socket, level, optname, optval, optlen);
}

/* { dg-final { scan-assembler "call\t49" } } */

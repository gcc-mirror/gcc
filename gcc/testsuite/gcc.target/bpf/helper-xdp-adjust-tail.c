/* { dg-do compile } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *xdp_md;
  int delta;

  ret = bpf_xdp_adjust_tail (xdp_md, delta);
}

/* { dg-final { scan-assembler "call\t65" } } */

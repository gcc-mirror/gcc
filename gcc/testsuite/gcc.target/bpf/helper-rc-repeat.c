/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx;

  ret = bpf_rc_repeat (ctx);
}

/* { dg-final { scan-assembler "call\t77" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx;
  int32_t rel_x, rel_y;

  ret = bpf_rc_pointer_rel (ctx, rel_x, rel_y);
}

/* { dg-final { scan-assembler "call\t92" } } */

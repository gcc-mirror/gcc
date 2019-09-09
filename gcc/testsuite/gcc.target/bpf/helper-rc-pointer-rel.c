/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx;
  int32_t rel_x, rel_y;

  ret = __builtin_bpf_helper_rc_pointer_rel (ctx, rel_x, rel_y);
}

/* { dg-final { scan-assembler "call\t92" } } */

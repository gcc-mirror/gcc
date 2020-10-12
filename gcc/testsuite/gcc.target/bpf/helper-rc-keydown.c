/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *ctx;
  uint32_t protocol, toggle;
  uint64_t scancode;

  ret = bpf_rc_keydown (ctx, protocol, scancode, toggle);
}

/* { dg-final { scan-assembler "call\t78" } } */

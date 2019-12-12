/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx;
  uint32_t protocol, toggle;
  uint64_t scancode;
  
  ret = __builtin_bpf_helper_rc_keydown (ctx, protocol,
					 scancode, toggle);
}

/* { dg-final { scan-assembler "call\t78" } } */

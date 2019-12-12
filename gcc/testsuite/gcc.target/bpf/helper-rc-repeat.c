/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *ctx;
  
  ret = __builtin_bpf_helper_rc_repeat (ctx);
}

/* { dg-final { scan-assembler "call\t77" } } */

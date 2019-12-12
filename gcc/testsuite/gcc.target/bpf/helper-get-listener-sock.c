/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ret, *sk;

  ret = __builtin_bpf_helper_get_listener_sock (sk);
}

/* { dg-final { scan-assembler "call\t98" } } */

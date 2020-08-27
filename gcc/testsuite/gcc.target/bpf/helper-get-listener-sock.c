/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *ret, *sk;

  ret = bpf_get_listener_sock (sk);
}

/* { dg-final { scan-assembler "call\t98" } } */

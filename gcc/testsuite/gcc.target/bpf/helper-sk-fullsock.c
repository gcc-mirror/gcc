/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ret, *sk;

  ret = __builtin_bpf_helper_sk_fullsock (sk);
}

/* { dg-final { scan-assembler "call\t95" } } */

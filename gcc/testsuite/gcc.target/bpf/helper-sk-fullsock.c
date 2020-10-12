/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *ret, *sk;

  ret = bpf_sk_fullsock (sk);
}

/* { dg-final { scan-assembler "call\t95" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *skb;

  bpf_set_hash_invalid (skb);
}

/* { dg-final { scan-assembler "call\t41" } } */

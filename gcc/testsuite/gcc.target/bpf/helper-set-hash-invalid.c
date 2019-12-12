/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *skb;

  __builtin_bpf_helper_set_hash_invalid (skb);
}

/* { dg-final { scan-assembler "call\t41" } } */

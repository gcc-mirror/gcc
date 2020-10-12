/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint32_t ret;
  void *skb;
  uint32_t hash;

  ret = bpf_set_hash (skb, hash);
}

/* { dg-final { scan-assembler "call\t48" } } */

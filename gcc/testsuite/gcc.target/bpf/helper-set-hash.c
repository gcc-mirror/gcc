/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint32_t ret;
  void *skb;
  uint32_t hash;
  
  ret = __builtin_bpf_helper_set_hash (skb, hash);
}

/* { dg-final { scan-assembler "call\t48" } } */

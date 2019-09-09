/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skops, *map, *key;
  uint64_t flags;

  ret = __builtin_bpf_helper_sock_hash_update (skops, map, key,
					       flags);
}

/* { dg-final { scan-assembler "call\t70" } } */

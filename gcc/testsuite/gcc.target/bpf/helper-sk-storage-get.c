/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ret;
  void *map, *sk, *value;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_sk_storage_get (map, sk, value,
					     flags);
}

/* { dg-final { scan-assembler "call\t107" } } */

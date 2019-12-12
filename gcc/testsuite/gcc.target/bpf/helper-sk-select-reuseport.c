/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *reuse, *map, *key;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_sk_select_reuseport (reuse, map,
						  key, flags);
}

/* { dg-final { scan-assembler "call\t82" } } */

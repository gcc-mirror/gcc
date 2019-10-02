/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ret, *map;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_get_local_storage (map, flags);
}

/* { dg-final { scan-assembler "call\t81" } } */

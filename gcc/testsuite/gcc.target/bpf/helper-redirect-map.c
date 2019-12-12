/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *map;
  uint32_t key;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_redirect_map (map, key, flags);
}

/* { dg-final { scan-assembler "call\t51" } } */

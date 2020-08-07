/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *map;
  uint32_t key;
  uint64_t flags;

  ret = bpf_redirect_map (map, key, flags);
}

/* { dg-final { scan-assembler "call\t51" } } */

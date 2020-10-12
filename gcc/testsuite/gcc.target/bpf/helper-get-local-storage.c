/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *ret, *map;
  uint64_t flags;

  ret = bpf_get_local_storage (map, flags);
}

/* { dg-final { scan-assembler "call\t81" } } */

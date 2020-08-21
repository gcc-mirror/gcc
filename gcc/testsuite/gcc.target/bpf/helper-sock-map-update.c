/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skops, *map, *key;
  uint64_t flags;

  ret = bpf_sock_map_update (skops, map, key, flags);
}

/* { dg-final { scan-assembler "call\t53" } } */

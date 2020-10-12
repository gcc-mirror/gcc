/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *reuse, *map, *key;
  uint64_t flags;

  ret = bpf_sk_select_reuseport (reuse, map,
				 key, flags);
}

/* { dg-final { scan-assembler "call\t82" } } */

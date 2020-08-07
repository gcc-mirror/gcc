/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *ret;
  void *map, *sk, *value;
  uint64_t flags;

  ret = bpf_sk_storage_get (map, sk, value, flags);
}

/* { dg-final { scan-assembler "call\t107" } } */

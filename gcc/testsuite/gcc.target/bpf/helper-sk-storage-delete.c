/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *map, *sk;

  ret = bpf_sk_storage_delete (map, sk);
}

/* { dg-final { scan-assembler "call\t108" } } */

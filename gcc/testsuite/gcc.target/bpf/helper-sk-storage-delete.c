/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *map, *sk;
  
  ret = __builtin_bpf_helper_sk_storage_delete (map, sk);
}

/* { dg-final { scan-assembler "call\t108" } } */

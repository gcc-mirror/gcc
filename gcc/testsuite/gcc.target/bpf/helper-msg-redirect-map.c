/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *msg, *map;
  uint64_t key;
  uint64_t flags;

  ret = bpf_msg_redirect_map (msg, map, key, flags);
}

/* { dg-final { scan-assembler "call\t60" } } */

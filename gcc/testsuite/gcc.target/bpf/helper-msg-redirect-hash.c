/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *msg, *map, *key;
  uint64_t flags;

  ret = __builtin_bpf_helper_msg_redirect_hash (msg, map, key,
						flags);
}

/* { dg-final { scan-assembler "call\t71" } } */

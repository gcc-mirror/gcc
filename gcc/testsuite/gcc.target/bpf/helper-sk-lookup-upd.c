/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  void *ret;
  void *ctx, *tuple;
  uint32_t tuple_size;
  uint64_t netns, flags;
  
  ret = __builtin_bpf_helper_sk_lookup_udp (ctx,
					    tuple,
					    tuple_size,
					    netns, flags);
}

/* { dg-final { scan-assembler "call\t85" } } */

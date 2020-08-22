/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  void *ret;
  void *ctx, *tuple;
  uint32_t tuple_size;
  uint64_t netns, flags;

  ret = bpf_sk_lookup_udp (ctx,
			   tuple,
			   tuple_size,
			   netns, flags);
}

/* { dg-final { scan-assembler "call\t85" } } */

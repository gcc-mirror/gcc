/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *to;
  uint32_t offset, len, start_header;

  ret = bpf_skb_load_bytes_relative (skb, offset,
				     to, len,
				     start_header);
}

/* { dg-final { scan-assembler "call\t68" } } */

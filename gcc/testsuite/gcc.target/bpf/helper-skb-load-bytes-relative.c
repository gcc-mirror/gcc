/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *to;
  uint32_t offset, len, start_header;

  ret = __builtin_bpf_helper_skb_load_bytes_relative (skb, offset,
						      to, len,
						      start_header);
}

/* { dg-final { scan-assembler "call\t68" } } */

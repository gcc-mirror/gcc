/* { dg-do compile } */

#include <stdint.h>
#include <stddef.h>

void
foo ()
{
  int ret;
  void *buf;
  unsigned long res;
  uint64_t flags;
  size_t buf_len;
  
  ret = __builtin_bpf_helper_strtoul (buf, buf_len, flags, &res);
}

/* { dg-final { scan-assembler "call\t106" } } */

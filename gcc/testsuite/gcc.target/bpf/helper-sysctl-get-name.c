/* { dg-do compile } */

#include <stdint.h>
#include <stddef.h>

void
foo ()
{
  int ret;
  void *ctx, *buf;
  size_t buf_len;
  uint64_t flags;
  
  ret = __builtin_bpf_helper_sysctl_get_name (ctx, buf,
					      buf_len, flags);
}

/* { dg-final { scan-assembler "call\t101" } } */

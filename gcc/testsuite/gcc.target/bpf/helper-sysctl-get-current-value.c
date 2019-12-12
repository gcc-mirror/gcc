/* { dg-do compile } */

#include <stdint.h>
#include <stddef.h>

void
foo ()
{
  int ret;
  void *ctx, *buf;
  size_t buf_len;
  
  ret = __builtin_bpf_helper_sysctl_get_current_value (ctx, buf,
						       buf_len);
}

/* { dg-final { scan-assembler "call\t102" } } */

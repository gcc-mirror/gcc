/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *buf;
  uint32_t size_of_buf;

  ret = __builtin_bpf_helper_get_current_comm (buf, size_of_buf);
}

/* { dg-final { scan-assembler "call\t16" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *buf;
  uint32_t size_of_buf;

  ret = bpf_get_current_comm (buf, size_of_buf);
}

/* { dg-final { scan-assembler "call\t16" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  int size;
  void *dst;
  const void *unsafe_ptr;

  ret = bpf_probe_read_str (dst, size, unsafe_ptr);
}

/* { dg-final { scan-assembler "call\t45" } } */

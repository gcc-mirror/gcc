/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  int size;
  void *dst;
  const void *unsafe_ptr;
  
  ret = __builtin_bpf_helper_probe_read_str (dst, size, unsafe_ptr);
}

/* { dg-final { scan-assembler "call\t45" } } */

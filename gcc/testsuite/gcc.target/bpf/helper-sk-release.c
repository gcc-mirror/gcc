/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *sock;
  
  ret = __builtin_bpf_helper_sk_release (sock);
}

/* { dg-final { scan-assembler "call\t86" } } */

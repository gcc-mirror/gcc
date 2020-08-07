/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *sock;

  ret = bpf_sk_release (sock);
}

/* { dg-final { scan-assembler "call\t86" } } */

/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  uint64_t ret;
  void *skb;

  ret = bpf_get_socket_cookie (skb);
}

/* { dg-final { scan-assembler "call\t46" } } */

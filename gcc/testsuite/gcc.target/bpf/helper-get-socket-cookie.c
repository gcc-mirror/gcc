/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  uint64_t ret;
  void *skb;
  
  ret = __builtin_bpf_helper_get_socket_cookie (skb);
}

/* { dg-final { scan-assembler "call\t46" } } */

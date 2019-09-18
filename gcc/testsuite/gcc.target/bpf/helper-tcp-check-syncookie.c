/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *sk, *iph, *th;
  uint32_t iph_len, th_len;
  
  ret = __builtin_bpf_helper_tcp_check_syncookie (sk, iph,
						  iph_len,
						  th, th_len);
}

/* { dg-final { scan-assembler "call\t100" } } */

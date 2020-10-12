/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *sk, *iph, *th;
  uint32_t iph_len, th_len;

  ret = bpf_tcp_check_syncookie (sk, iph,
				 iph_len,
				 th, th_len);
}

/* { dg-final { scan-assembler "call\t100" } } */

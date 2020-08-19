/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

#include <stdint.h>
#include <bpf-helpers.h>

void
foo ()
{
  int ret;
  void *skb, *param;
  uint32_t action, param_len;

  ret = bpf_lwt_seg6_action (skb, action,
			     param, param_len);
}

/* { dg-final { scan-assembler "call\t76" } } */

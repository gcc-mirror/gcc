/* { dg-do compile } */

#include <stdint.h>

void
foo ()
{
  int ret;
  void *skb, *param;
  uint32_t action, param_len;
  
  ret = __builtin_bpf_helper_lwt_seg6_action (skb, action,
					      param, param_len);
}

/* { dg-final { scan-assembler "call\t76" } } */

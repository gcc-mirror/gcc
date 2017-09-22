/* { dg-do compile } */
/* { dg-options "-O2 -fstack-clash-protection" } */
/* { dg-require-effective-target supports_stack_clash_protection } */

#include <stdint.h>

extern void arf (uint64_t *, uint64_t *);
void
frob ()
{
  uint64_t num[859];
  uint64_t den[859];
  arf (den, num);
}

/* { dg-final { scan-assembler-times "sub\[ql\]" 4 } } */
/* { dg-final { scan-assembler-times "or\[ql\]" 3 } } */


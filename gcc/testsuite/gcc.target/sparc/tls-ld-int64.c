/* { dg-do compile } */
/* { dg-options "-O2" }
/* { dg-add-options tls } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

__thread int64_t var1 __attribute__((tls_model("local-dynamic")));
__thread int64_t var2 __attribute__((tls_model("local-dynamic")));

int64_t sum (void)
{
  return var1 + var2;
}

void set (int64_t i)
{
  var1 = i;
  var2 = i;
}

/* { dg-final { scan-assembler-times "__tls_get_addr" 2 } } */
/* { dg-final { scan-assembler-times "ldx\t\[^\n\]*tldo_add" 2 } } */
/* { dg-final { scan-assembler-times "stx\t\[^\n\]*tldo_add" 2 } } */

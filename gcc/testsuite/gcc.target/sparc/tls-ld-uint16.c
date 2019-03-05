/* { dg-do compile } */
/* { dg-options "-O2" }
/* { dg-add-options tls } */

#include <stdint.h>

__thread uint16_t var1 __attribute__((tls_model("local-dynamic")));
__thread uint16_t var2 __attribute__((tls_model("local-dynamic")));

uint16_t sum (void)
{
  return var1 + var2;
}

uint32_t ext32_sum (void)
{
  return (uint32_t)var1 + (uint32_t)var2;
}

unsigned long ext_sum (void)
{
  return (unsigned long)var1 + (unsigned long)var2;
}

void set (uint16_t i)
{
  var1 = i;
  var2 = i;
}

/* { dg-final { scan-assembler-times "__tls_get_addr" 4 } } */
/* { dg-final { scan-assembler-times "lduh\t\[^\n\]*tldo_add" 6 } } */
/* { dg-final { scan-assembler-times "sth\t\[^\n\]*tldo_add" 2 } } */

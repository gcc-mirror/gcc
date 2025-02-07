/* { dg-do compile } */
/* { dg-options "-O2" }
/* { dg-add-options tls } */
/* { dg-skip-if "native tls expected" { tls_emulated } } */

#include <stdint.h>

__thread uint8_t var1 __attribute__((tls_model("local-dynamic")));
__thread uint8_t var2 __attribute__((tls_model("local-dynamic")));

uint8_t sum (void)
{
  return var1 + var2;
}

uint16_t ext16_sum (void)
{
  return (uint16_t)var1 + (uint16_t)var2;
}

uint32_t ext32_sum (void)
{
  return (uint32_t)var1 + (uint32_t)var2;
}

unsigned long ext_sum (void)
{
  return (unsigned long)var1 + (unsigned long)var2;
}

void set (uint8_t i)
{
  var1 = i;
  var2 = i;
}

/* { dg-final { scan-assembler-times "__tls_get_addr" 5 } } */
/* { dg-final { scan-assembler-times "ldub\t\[^\n\]*tldo_add" 8 } } */
/* { dg-final { scan-assembler-times "stb\t\[^\n\]*tldo_add" 2 } } */

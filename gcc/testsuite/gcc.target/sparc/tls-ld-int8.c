/* { dg-do compile } */
/* { dg-options "-O2" }
/* { dg-add-options tls } */

#include <stdint.h>

__thread int8_t var1 __attribute__((tls_model("local-dynamic")));
__thread int8_t var2 __attribute__((tls_model("local-dynamic")));

int8_t sum (void)
{
  return var1 + var2;
}

int16_t ext16_sum (void)
{
  return (int16_t)var1 + (int16_t)var2;
}

int32_t ext32_sum (void)
{
  return (int32_t)var1 + (int32_t)var2;
}

long ext_sum (void)
{
  return (long)var1 + (long)var2;
}

void set (int8_t i)
{
  var1 = i;
  var2 = i;
}

/* { dg-final { scan-assembler-times "__tls_get_addr" 5 } } */
/* { dg-final { scan-assembler-times "ldub\t\[^\n\]*tldo_add" 2 } } */
/* { dg-final { scan-assembler-times "ldsb\t\[^\n\]*tldo_add" 6 } } */
/* { dg-final { scan-assembler-times "stb\t\[^\n\]*tldo_add" 2 } } */

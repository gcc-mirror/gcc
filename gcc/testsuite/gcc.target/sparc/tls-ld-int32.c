/* { dg-do compile } */
/* { dg-options "-O2" }
/* { dg-add-options tls } */
/* { dg-skip-if "native tls expected" { tls_emulated } } */

#include <stdint.h>

__thread int32_t var1 __attribute__((tls_model("local-dynamic")));
__thread int32_t var2 __attribute__((tls_model("local-dynamic")));

int32_t sum (void)
{
  return var1 + var2;
}

#if defined(__sparcv9) || defined(__arch64__)
long ext_sum (void)
{
  return (long)var1 + (long)var2;
}
#else
void *addr (void)
{
  return &var1;
}
#endif

void set (int32_t i)
{
  var1 = i;
  var2 = i;
}

/* { dg-final { scan-assembler-times "__tls_get_addr" 3 } } */
/* { dg-final { scan-assembler-times "ld\t\[^\n\]*tldo_add" 2 } } */
/* { dg-final { scan-assembler-times "ldsw\t\[^\n\]*tldo_add" 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times "st\t\[^\n\]*tldo_add" 2 } } */

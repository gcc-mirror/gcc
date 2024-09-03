/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main+mve+pacbti -mbranch-protection=pac-ret -mthumb -mfloat-abi=hard -fasynchronous-unwind-tables -g -O0" } */

#include "stdio.h"

__attribute__((noinline)) int
fn1 (int a)
{
  const char *fmt = "branch-protection";
  int fun1(int x,const char *fmt,int c,int d)
    {
      printf("string = %s\n",fmt);
      return x+c+d;
    }
  return fun1(a,fmt,10,10);
}

int main (void)
{
  return fn1 (40);
}

/* { dg-final { scan-assembler-times "\.pacspval" 1 } } */
/* { dg-final { scan-assembler-times "\tpac\tip, lr, sp" 3 } } */
/* { dg-final { scan-assembler-times "\.cfi_register 143, 12" 3 } } */
/* { dg-final { scan-assembler-times "\.save {r7, ra_auth_code, lr}" 2 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 143, -8" 2 } } */
/* { dg-final { scan-assembler-times "\.save {r3, r7, ra_auth_code, lr}" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 143, -12" 1 } } */

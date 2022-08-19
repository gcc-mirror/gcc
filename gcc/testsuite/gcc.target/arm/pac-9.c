/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -mbranch-protection=pac-ret+leaf+bti -mthumb -mfloat-abi=soft -fasynchronous-unwind-tables --save-temps -g" } */

__attribute__((noinline)) void
fn1 (int a, int b, int c)
{
  if (a != b + c)
    __builtin_abort ();
}

int main ()
{
  fn1 (40, 40, 80);
  return 0;
}

/* { dg-final { scan-assembler "\.save \{r7, ra_auth_code, lr\}" } } */
/* { dg-final { scan-assembler "\.save \{r3, r7, ra_auth_code, lr\}" } } */
/* { dg-final { scan-assembler "\.cfi_offset 143, \-8" } } */

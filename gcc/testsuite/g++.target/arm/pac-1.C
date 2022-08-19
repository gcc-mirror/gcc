/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main -mbranch-protection=pac-ret+leaf+bti -mthumb -mfloat-abi=soft --save-temps -g" } */

__attribute__((noinline)) void
fn1 (int a, int b, int c)
{
  if (a != b + c)
    __builtin_abort ();
  else
    throw b+c;
}

int main ()
{
  int a = 120;
  try
    {
      fn1 (a, 40, 80);
    }
  catch (int x)
    {
      if (x != a)
        __builtin_abort ();
      else
	return 0;
    }
}

/* { dg-final { scan-assembler "\.save \{r7, ra_auth_code, lr\}" } } */
/* { dg-final { scan-assembler "\.save \{r4, r7, ra_auth_code, lr\}" } } */
/* { dg-final { scan-assembler "\.cfi_offset 143, \-8" } } */

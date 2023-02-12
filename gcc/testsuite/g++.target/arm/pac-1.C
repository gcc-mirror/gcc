/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-skip-if "avoid conflicting multilib options" { *-*-* } { "-marm" "-mcpu=*" } } */
/* { dg-options "-march=armv8.1-m.main+mve+pacbti -mbranch-protection=pac-ret -mthumb -mfloat-abi=hard -g -O0" } */

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

/* { dg-final { scan-assembler-times "pac	ip, lr, sp" 2 } } */
/* { dg-final { scan-assembler-times "\.cfi_register 143, 12" 2 } } */
/* { dg-final { scan-assembler-times "\.save {r7, ra_auth_code, lr}" 1 } } */
/* { dg-final { scan-assembler-times "\.cfi_offset 143, -8" 2 } } */
/* { dg-final { scan-assembler-times "\.save {r4, r7, ra_auth_code, lr}" 1 } } */

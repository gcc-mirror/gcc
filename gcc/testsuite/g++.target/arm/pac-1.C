/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_pacbti_ok } */
/* { dg-add-options arm_arch_v8_1m_main_pacbti } */
/* { dg-additional-options "-mbranch-protection=pac-ret -mfloat-abi=hard -g -O0" } */

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

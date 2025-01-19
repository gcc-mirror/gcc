/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8_1m_main_ok } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-mbranch-protection=pac-ret+leaf --save-temps -O0 -g" } */
/* { dg-add-options arm_arch_v8_1m_main } */

int i;

void foo (int);

int bar()
{
  foo (i);
  return 0;
}

/* { dg-final { scan-assembler "\tpac\tip, lr, sp" } } */
/* { dg-final { scan-assembler "\taut\tip, lr, sp" } } */
/* { dg-final { scan-assembler-not "\tbti" } } */

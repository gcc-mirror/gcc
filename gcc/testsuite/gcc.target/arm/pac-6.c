/* Check that GCC does .save and .cfi_offset directives with RA_AUTH_CODE pseudo hard-register.  */
/* { dg-do compile } */
/* { dg-require-effective-target mbranch_protection_ok } */
/* { dg-options "-march=armv8.1-m.main+fp -mbranch-protection=pac-ret+leaf -mthumb --save-temps -O0 -g" } */

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

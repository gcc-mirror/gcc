/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-require-effective-target powerpc_p9modulo_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */
/* { dg-options "-mcpu=power9 -O2" } */

long
func1 (int reg, int *is_zero)
{
  long value;

  __asm__ (" #foo %0" : "+r" (reg));
  value = ((long)reg) << 4;

  if (!value)
    *is_zero = 1;

  return value;
}

long
func2 (int *ptr, int *is_zero)
{
  int reg = *ptr;
  long value = ((long)reg) << 4;

  if (!value)
    *is_zero = 1;

  return value;
}

/* { dg-final { scan-assembler     "extswsli\\. " } } */
/* { dg-final { scan-assembler     "lwz "         } } */
/* { dg-final { scan-assembler-not "lwa "         } } */
/* { dg-final { scan-assembler-not "sldi "        } } */
/* { dg-final { scan-assembler-not "sldi\\. "     } } */
/* { dg-final { scan-assembler-not "extsw "       } } */

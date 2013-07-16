/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O3 -mcpu=power7 -fcaller-saves" } */
/* { dg-final { scan-assembler-not "lxvd2x" } } */
/* { dg-final { scan-assembler-not "lxvw4x" } } */
/* { dg-final { scan-assembler-not "lvx" } } */
/* { dg-final { scan-assembler-not "stxvd2x" } } */
/* { dg-final { scan-assembler-not "stxvw4x" } } */
/* { dg-final { scan-assembler-not "stvx" } } */

/* Insure caller save on long double does not use VSX instructions.  */

extern long double modify (long double);

void
sum (long double *ptr, long double value, unsigned long n)
{
  unsigned long i;

  for (i = 0; i < n; i++)
    ptr[i] += modify (value);
}

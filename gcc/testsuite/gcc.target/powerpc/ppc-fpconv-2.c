/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power6 -ffast-math" } */
/* { dg-final { scan-assembler-times "lfiwax" 2 } } */
/* { dg-final { scan-assembler-not "lfiwzx" } } */
/* { dg-final { scan-assembler-times "fcfid " 10 } } */
/* { dg-final { scan-assembler-not "fcfids" } } */
/* { dg-final { scan-assembler-not "fcfidus" } } */
/* { dg-final { scan-assembler-not "xscvsxddp" } } */
/* { dg-final { scan-assembler-not "xscvuxddp" } } */

void int_to_float (float *dest, int *src)
{
  *dest = (float) *src;
}

void int_to_double (double *dest, int *src)
{
  *dest = (double) *src;
}

void uint_to_float (float *dest, unsigned int *src)
{
  *dest = (float) *src;
}

void uint_to_double (double *dest, unsigned int *src)
{
  *dest = (double) *src;
}

void llong_to_float (float *dest, long long *src)
{
  *dest = (float) *src;
}

void llong_to_double (double *dest, long long *src)
{
  *dest = (double) *src;
}

void ullong_to_float (float *dest, unsigned long long *src)
{
  *dest = (float) *src;
}

void ullong_to_double (double *dest, unsigned long long *src)
{
  *dest = (double) *src;
}

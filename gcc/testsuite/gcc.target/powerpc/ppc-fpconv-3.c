/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target powerpc_fprs } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power5" } } */
/* { dg-options "-O2 -mcpu=power5 -ffast-math" } */
/* { dg-final { scan-assembler-not "lfiwax" } } */
/* { dg-final { scan-assembler-not "lfiwzx" } } */
/* { dg-final { scan-assembler-times "fcfid " 10 } } */
/* { dg-final { scan-assembler-not "fcfids " } } */
/* { dg-final { scan-assembler-not "fcfidus " } } */
/* { dg-final { scan-assembler-not "fcfidu " } } */
/* { dg-final { scan-assembler-not "xscvsxddp " } } */
/* { dg-final { scan-assembler-not "xscvuxddp " } } */

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

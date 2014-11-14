/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -ffast-math -mno-upper-regs-df" } */
/* { dg-final { scan-assembler-times "lfiwax" 2 } } */
/* { dg-final { scan-assembler-times "lfiwzx" 2 } } */
/* { dg-final { scan-assembler-times "fcfids " 3 } } */
/* { dg-final { scan-assembler-times "fcfidus " 1 } } */
/* { dg-final { scan-assembler-times "fcfid " 3 } } */
/* { dg-final { scan-assembler-times "fcfidu " 1 } } */
/* { dg-final { scan-assembler-not "xscvdpsxds" } } */
/* { dg-final { scan-assembler-not "xscvdpuxds" } } */

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

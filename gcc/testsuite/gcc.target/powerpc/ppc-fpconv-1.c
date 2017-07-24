/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power7" } } */
/* { dg-options "-O2 -mcpu=power7 -ffast-math" } */
/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}    2 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}    2 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp\M}  3 } } */
/* { dg-final { scan-assembler-times {\mfcfidus\M|\mxscvuxdsp\M} 1 } } */
/* { dg-final { scan-assembler-times {\mfcfid\M|\mxscvsxddp\M}   3 } } */
/* { dg-final { scan-assembler-times {\mfcfidu\M|\mxscvuxddp\M}  1 } } */

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

/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power7" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-times {\mstfiwx\M|\mstxsiwx\M}     4 } } */
/* { dg-final { scan-assembler-times {\mlfiwax\M|\mlxsiwax\M}     2 } } */
/* { dg-final { scan-assembler-times {\mlfiwzx\M|\mlxsiwzx\M}     2 } } */
/* { dg-final { scan-assembler-times {\mfctiwz\M|\mxscvdpsxws\M}  2 } } */
/* { dg-final { scan-assembler-times {\mfctiwuz\M|\mxscvdpuxws\M} 2 } } */
/* { dg-final { scan-assembler-times {\mfcfids\M|\mxscvsxdsp\M}   2 } } */
/* { dg-final { scan-assembler-not   {\mlwz\M}                      } } */
/* { dg-final { scan-assembler-not   {\mstw\M}                      } } */

/* Make sure we don't have loads/stores to the GPR unit.  */
double
round_double_int (double a)
{
  return (double)(int)a;
}

float
round_float_int (float a)
{
  return (float)(int)a;
}

double
round_double_uint (double a)
{
  return (double)(unsigned int)a;
}

float
round_float_uint (float a)
{
  return (float)(unsigned int)a;
}

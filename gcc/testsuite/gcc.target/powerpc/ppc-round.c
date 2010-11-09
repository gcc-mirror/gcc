/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mcpu=power7" } */
/* { dg-final { scan-assembler-times "stfiwx" 4 } } */
/* { dg-final { scan-assembler-times "lfiwax" 2 } } */
/* { dg-final { scan-assembler-times "lfiwzx" 2 } } */
/* { dg-final { scan-assembler-times "fctiwz" 2 } } */
/* { dg-final { scan-assembler-times "xscvsxddp" 2 } } */
/* { dg-final { scan-assembler-times "fcfids" 2 } } */
/* { dg-final { scan-assembler-not "lwz" } } */
/* { dg-final { scan-assembler-not "stw" } } */

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

/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power8" } */
/* { dg-final { scan-assembler-times "fcfid "      2 } } */
/* { dg-final { scan-assembler-times "fcfids "     2 } } */
/* { dg-final { scan-assembler-times "fctiwuz "    2 } } */
/* { dg-final { scan-assembler-times "fctiwz "     2 } } */
/* { dg-final { scan-assembler-times "mfvsrd "     4 } } */
/* { dg-final { scan-assembler-times "mtvsrwa "    2 } } */
/* { dg-final { scan-assembler-times "mtvsrwz "    2 } } */
/* { dg-final { scan-assembler-not   "lwz"           } } */
/* { dg-final { scan-assembler-not   "lfiwax "       } } */
/* { dg-final { scan-assembler-not   "lfiwzx "       } } */
/* { dg-final { scan-assembler-not   "stw"           } } */
/* { dg-final { scan-assembler-not   "stfiwx "       } } */

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

/* { dg-do compile { target { powerpc*-*-* && lp64 } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } { "*" } { "" } } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power8" } } */
/* { dg-options "-O2 -mcpu=power9" } */
/* { dg-final { scan-assembler-times "fcfid \|xscvsxddp "    2 } } */
/* { dg-final { scan-assembler-times "fcfids \|xscvsxdsp "   2 } } */
/* { dg-final { scan-assembler-times "fctiwz \|xscvdpsxws "  2 } } */
/* { dg-final { scan-assembler-times "fctiwuz \|xscvdpuxws " 2 } } */
/* { dg-final { scan-assembler-times {\mvextsw2d\M}          2 } } */
/* { dg-final { scan-assembler-times {\mxxextractuw\M}       2 } } */
/* { dg-final { scan-assembler-not   {\mmfvsrd\M}              } } */
/* { dg-final { scan-assembler-not   {\mmfvsrwz\M}             } } */
/* { dg-final { scan-assembler-not   {\mmtvsrwa\M}             } } */
/* { dg-final { scan-assembler-not   {\mmtvsrwz\M}             } } */
/* { dg-final { scan-assembler-not   {\mlwz\M}                 } } */
/* { dg-final { scan-assembler-not   {\mlfiwax\M}              } } */
/* { dg-final { scan-assembler-not   {\mlfiwzx\M}              } } */
/* { dg-final { scan-assembler-not   {\mstw\M}                 } } */
/* { dg-final { scan-assembler-not   {\mstfiwx\M}              } } */

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

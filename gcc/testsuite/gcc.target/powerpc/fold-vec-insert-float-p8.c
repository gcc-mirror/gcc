/* Verify that overloaded built-ins for vec_insert with float
   inputs produce the right codegen.  Power8 variant.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

#include <altivec.h>

vector float
testf_var (float f, vector float vf, signed int i)
{
  return vec_insert (f, vf, i);
}

vector float
testf_cst (float f, vector float vf)
{
  return vec_insert (f, vf, 12);
}

/* { dg-final { scan-assembler-times {\mstvx\M|\mstxv\M|\mstxvd2x\M} 1 } } */
/* cst tests has stfs instead of stfsx. */
/* { dg-final { scan-assembler-times {\mstfs\M|\mstfsx\M} 2 } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxv\M|\mlxvd2x\M|\mlxvw4x\M} 2 } } */

/* cst test has a lvewx,vperm combo */
/* { dg-final { scan-assembler-times {\mlvewx\M} 1 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 1 } } */


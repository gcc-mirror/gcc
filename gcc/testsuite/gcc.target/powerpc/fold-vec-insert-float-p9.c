/* Verify that overloaded built-ins for vec_insert with float
   inputs produce the right codegen.  Power9 variant.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

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

/* var test has a load and store. */
/* { dg-final { scan-assembler-times {\mlxv\M|\mlvx\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstfsx\M} 0 { target lp64} } } */

/* cst test have a xscvdpspn,xxextractuw,xxinsertw combo */
/* { dg-final { scan-assembler-times {\mxscvdpspn\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxextractuw\M} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxinsertw\M} 2 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mstfs\M} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlvewx\M} 1 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 2 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxperm\M} 1 { target ilp32 } } } */

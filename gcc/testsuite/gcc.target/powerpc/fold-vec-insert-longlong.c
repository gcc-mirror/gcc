/* Verify that overloaded built-ins for vec_insert() with long long
   inputs produce the right codegen.  */

/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector bool long long
testbl_var(unsigned long long x, vector bool long long v, signed int i)
{
   return vec_insert(x, v, i);
}

vector signed long long
testsl_var(signed long long x, vector signed long long v, signed int i)
{
   return vec_insert(x, v, i);
}

vector unsigned long long
testul1_var(signed long long x, vector unsigned long long v, signed int i)
{
   return vec_insert(x, v, i);
}

vector unsigned long long
testul2_var(unsigned long long x, vector unsigned long long v, signed int i)
{
   return vec_insert(x, v, i);
}

vector bool long long
testbl_cst(unsigned long long x, vector bool long long v)
{
   return vec_insert(x, v, 12);
}

vector signed long long
testsl_cst(signed long long x, vector signed long long v)
{
   return vec_insert(x, v, 12);
}

vector unsigned long long
testul1_cst(signed long long x, vector unsigned long long v)
{
   return vec_insert(x, v, 12);
}

vector unsigned long long
testul2_cst(unsigned long long x, vector unsigned long long v)
{
   return vec_insert(x, v, 12);
}

/* Number of xxpermdi insns varies between power targets.  ensure at least one. */
/* { dg-final { scan-assembler {\mxxpermdi\M} } } */

/* { dg-final { scan-assembler-times {\mrldic\M|\mrlwinm\M} 4 } } */

/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstvx\M|\mstxv\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstdx\M} 0 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxv\M|\mlvx\M} 0 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstvx\M|\mstxv\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxv\M|\mlvx\M} 0 { target ilp32 } } } */

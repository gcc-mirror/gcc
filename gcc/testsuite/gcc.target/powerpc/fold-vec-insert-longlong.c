/* Verify that overloaded built-ins for vec_insert() with long long
   inputs produce the right codegen.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

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

/* The number of addi instructions decreases on newer systems.  Measured as 8 on
 power7 and power8 targets, and drops to 4 on power9 targets that use the
 newer stxv,lxv instructions.  For this test ensure we get at least one.  */
/* { dg-final { scan-assembler {\maddi\M} } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M|\mstvx\M|\mstxv\M} 4 } } */
/* { dg-final { scan-assembler-times {\mstdx\M} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstw\M} 8 { target ilp32 } } } */

/* { dg-final { scan-assembler-times {\mlxvd2x\M|\mlxv\M|\mlvx\M} 4 } } */


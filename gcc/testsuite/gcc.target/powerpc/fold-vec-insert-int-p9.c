/* Verify that overloaded built-ins for vec_insert() with int
   inputs produce the right codegen.  Power9 variant.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

vector bool int
testbi_var(unsigned int x, vector bool int v, signed int i)
{
   return vec_insert(x, v, i);
}
vector signed int
testsi_var(signed int x, vector signed int v, signed int i)
{
   return vec_insert(x, v, i);
}
vector unsigned int
testui1_var(signed int x, vector unsigned int v, signed int i)
{
   return vec_insert(x, v, i);
}
vector unsigned int
testui2_var(unsigned int x, vector unsigned int v, signed int i)
{
   return vec_insert(x, v, i);
}
vector bool int
testbi_cst(unsigned int x, vector bool int v)
{
   return vec_insert(x, v, 12);
}
vector signed int
testsi_cst(signed int x, vector signed int v)
{
   return vec_insert(x, v, 12);
}
vector unsigned int
testui1_cst(signed int x, vector unsigned int v)
{
   return vec_insert(x, v, 12);
}
vector unsigned int
testui2_cst(unsigned int x, vector unsigned int v)
{
   return vec_insert(x, v, 12);
}


/* { dg-final { scan-assembler-times {\mstxv\M|\mstvx\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstwx\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M|\mlvx\M} 0 { target lp64 } } } */

/* an insert and a move per constant test. */
/* { dg-final { scan-assembler-times {\mmtvsrwz\M} 8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mxxinsertw\M} 8 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mstxv\M|\mstvx\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstw\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlvewx\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxperm\M} 8 { target ilp32 } } } */

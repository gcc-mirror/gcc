/* Verify that overloaded built-ins for vec_insert() with int
   inputs produce the right codegen.  Power8 variant.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */

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

/* Each test has lvx (8).  cst tests have additional lvewx. (4) */
/* var tests have no stwx and stvx.  cst tests have stw (4).*/
/* { dg-final { scan-assembler-times {\mstvx\M|\mstwx\M|\mstw\M|\mstxvw4x\M} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvw4x\M} 8 { target le } } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvw4x\M} 4 { target { be && lp64 } } } } */

/* { dg-final { scan-assembler-times {\mlvewx\M} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 12 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mvperm\M} 12 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstvx\M|\mstwx\M|\mstw\M|\mstxvw4x\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvw4x\M} 4 { target { be && ilp32 } } } } */
/* { dg-final { scan-assembler-times {\mlvewx\M} 4 { target ilp32 } } } */

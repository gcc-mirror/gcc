/* Verify that overloaded built-ins for vec_insert() with short
   inputs produce the right codegen.  Power9 variant.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -mvsx" } */

#include <altivec.h>

vector bool short
testbs_var(unsigned short x, vector bool short v, signed int i)
{
   return vec_insert(x, v, i);
}
vector signed short
testss_var(signed short x, vector signed short v, signed int i)
{
   return vec_insert(x, v, i);
}
vector unsigned short
testus1_var(signed short x, vector unsigned short v, signed int i)
{
   return vec_insert(x, v, i);
}
vector unsigned short
testus2_var(unsigned short x, vector unsigned short v, signed int i)
{
   return vec_insert(x, v, i);
}
vector bool short
testbs_cst(signed short x, vector bool short v)
{
   return vec_insert(x, v, 12);
}
vector signed short
testss_cst(signed short x, vector signed short v)
{
   return vec_insert(x, v, 12);
}
vector unsigned short
testus1_cst(signed short x, vector unsigned short v)
{
   return vec_insert(x, v, 12);
}
vector unsigned short
testus2_cst(unsigned short x, vector unsigned short v)
{
   return vec_insert(x, v, 12);
}

/* { dg-final { scan-assembler-times {\mmtvsrwz\M} 8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvinserth\M} 8 { target lp64 } } } */

/* { dg-final { scan-assembler-times {\mstxv\M|\mstvx\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M|\mlvx\M} 0 { target lp64 }} } */

/* -m32 uses sth/lvehx as part of the sequence. */
/* { dg-final { scan-assembler-times {\mstxv\M|\mstvx\M} 0 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\msth\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlvehx\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxperm\M} 8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M|\mlvx\M} 8 { target ilp32 } } } */


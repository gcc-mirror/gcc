/* Verify that overloaded built-ins for vec_insert() with short
   inputs produce the right codegen.  Power8 variant.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

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

/* { dg-final { scan-assembler-times {\mlhz\M|\mlvx\M|\mlxv\M|\mlxvw4x\M} 8 } } */
/* stores.. 2 each per variable tests, 1 each per cst test. */
/* { dg-final { scan-assembler-times {\msthx\M|\mstvx\M|\msth\M|\mstxvw4x\M} 12 } } */

/* { dg-final { scan-assembler-times {\mlvehx\M} 4 } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 4 } } */


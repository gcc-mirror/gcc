/* Verify that overloaded built-ins for vec_insert () with char
   inputs produce the right codegen.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power9" } */

/* The below contains vec_insert () calls with both variable and constant
 values.  Only the constant value calls are early-gimple folded, but all
 are tested for coverage.  */

#include <altivec.h>

vector bool char testub_var (unsigned char x, vector bool char v, signed int i)
{
       return vec_insert (x, v, i);
}
vector signed char testss_var (signed char x, vector signed char v, signed int i)
{
       return vec_insert (x, v, i);
}
vector unsigned char testsu_var (signed char x, vector unsigned char v, signed int i)
{
       return vec_insert (x, v, i);
}
vector unsigned char testuu_var (unsigned char x, vector unsigned char v, signed int i)
{
       return vec_insert (x, v, i);
}
vector bool char testub_cst  (unsigned char x, vector bool char v)
{
       return vec_insert (x, v, 12);
}
vector signed char testss_cst  (signed char x, vector signed char v)
{
       return vec_insert (x, v, 12);
}
vector unsigned char testsu_cst (signed char x, vector unsigned char v)
{
       return vec_insert (x, v, 12);
}
vector unsigned char testuu_cst (unsigned char x, vector unsigned char v)
{
       return vec_insert (x, v, 12);
}

/* no store per _var test.  */
/* { dg-final { scan-assembler-times {\mstxv\M|\mstvx\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mstb\M} 0 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mlvebx\M|\mlxv\M|\mlvx\M} 0 { target lp64} } } */
/* an insert and a move per constant test. */
/* { dg-final { scan-assembler-times {\mmtvsrwz\M} 8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\mvinsertb\M} 8 { target lp64 } } } */

/* -m32 codegen. */
/* { dg-final { scan-assembler-times {\mrlwinm\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mstb\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlxv\M} 8 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mlvebx\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mvperm\M} 4 { target ilp32 } } } */
/* { dg-final { scan-assembler-times {\mxxperm\M} 8 { target ilp32 } } } */

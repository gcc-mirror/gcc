/* Verify that overloaded built-ins for vec_insert () with char
   inputs produce the right codegen.  */

/* { dg-do compile { target { powerpc*-*-linux* } } } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

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

/* one store per _var test */
/* { dg-final { scan-assembler-times {\mstvx\M|\mstxvw4x\M} 4 } } */
/* one store-byte per test */
/* { dg-final { scan-assembler-times {\mstb\M} 8 } } */
/* one load per test */
/* { dg-final { scan-assembler-times {\mlvx\M|\mlxvw4x\M} 8 } } */

/* one lvebx per _cst test.*/
/* { dg-final { scan-assembler-times {\mlvebx\M} 4 } } */
/* one vperm per _cst test.*/
/* { dg-final { scan-assembler-times {\mvperm\M} 4 } } */


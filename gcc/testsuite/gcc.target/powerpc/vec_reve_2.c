/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -O2" } */

#include <altivec.h>

vector int foo1 (vector int a)
{
   return vec_reve (a);
}

vector float foo2 (vector float a)
{
   return vec_reve (a);
}

vector short foo3 (vector short a)
{
   return vec_reve (a);
}

vector char foo4 (vector char a)
{
   return vec_reve (a);
}

/* { dg-final { scan-assembler-times {\mxxbrq\M} 4 } } */
/* { dg-final { scan-assembler-times {\mxxbrw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxbrh\M} 1 } } */

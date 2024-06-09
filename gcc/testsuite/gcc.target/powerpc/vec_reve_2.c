/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

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

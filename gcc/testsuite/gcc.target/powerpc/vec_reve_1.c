/* { dg-do compile } */
/* { dg-options "-O2 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector double foo1 (vector double a)
{
   return vec_reve (a);
}

vector long long foo2 (vector long long a)
{
   return vec_reve (a);
}

/* { dg-final { scan-assembler-times {\mxxpermdi\M} 2 } } */

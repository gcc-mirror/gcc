/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler-not {\mxxlnor\M} } } */

#include <altivec.h>

vector unsigned int revb (vector unsigned int a)
{
   return vec_revb(a);
}

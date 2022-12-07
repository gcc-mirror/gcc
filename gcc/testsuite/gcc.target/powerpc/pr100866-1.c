/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */
/* { dg-final { scan-assembler-not {\mxxlnor\M} } } */

#include <altivec.h>

vector unsigned int revb (vector unsigned int a)
{
   return vec_revb(a);
}

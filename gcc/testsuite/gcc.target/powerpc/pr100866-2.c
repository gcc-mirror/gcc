/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power8 -mvsx" } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-final { scan-assembler {\mvspltish\M} } } */
/* { dg-final { scan-assembler {\mvrlh\M} } } */

#include <altivec.h>

vector unsigned short revb(vector unsigned short a)
{
   return vec_revb(a);
}


/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-final { scan-assembler {\mvspltisw\M} } } */
/* { dg-final { scan-assembler {\mvupkhsw\M} } } */
/* { dg-final { scan-assembler-not {\mlvx\M} } } */

#include <altivec.h>

vector unsigned long long 
foo ()
{
  return vec_splats ((unsigned long long) 12);
}

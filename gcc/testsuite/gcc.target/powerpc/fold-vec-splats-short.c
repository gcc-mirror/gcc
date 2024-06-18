/* Verify that overloaded built-ins for vec_splat with short
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed short
test3s (signed short x)
{
  return vec_splats (x);
}

vector unsigned short
test3u (unsigned short x)
{
  return vec_splats (x);
}

/* { dg-final { scan-assembler-times "vsplth" 2 } } */


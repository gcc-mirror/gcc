/* Verify that overloaded built-ins for vec_splat with long long
   inputs produce the right code.  */

/* { dg-do compile { target lp64 } } */
/* { dg-options "-mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed long long
test3s (signed long long x)
{
  return vec_splats (x);
}

vector unsigned long long
test3u (unsigned long long x)
{
  return vec_splats (x);
}

/* { dg-final { scan-assembler-times "xxpermdi|mtvsrdd" 2 } } */

/* Verify that overloaded built-ins for vec_splat with int
   inputs produce the right code.  */

/* { dg-do compile { target lp64 } } */
/* { dg-require-effective-target powerpc_altivec_ok } */
/* { dg-options "-maltivec -O2 " } */

#include <altivec.h>

vector signed int
test3s (signed int x)
{
  return vec_splats (x);
}

vector unsigned int
test3u (unsigned int x)
{
  return vec_splats (x);
}

/* { dg-final { scan-assembler-times {\mvspltw\M|\mxxspltw\M|\mmtvsrws\M} 2 } } */

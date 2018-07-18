/* Verify that overloaded built-ins for vec_abs with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2 -fwrapv" } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x)
{
  return vec_abs (x);
}

/* scan-assembler stanzas moved to fold-vec-abs-longlong.p*.c.  */


/* Verify that overloaded built-ins for vec_neg with int
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -O2" } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_neg (x);
}

/* Scan-assembler stanzas have been moved to fold-vec-neg-int.p*.c tests. */

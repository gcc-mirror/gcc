/* Verify that overloaded built-ins for vec_abs with int
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-maltivec -O2 -fwrapv" } */
/* { dg-require-effective-target powerpc_altivec } */

#include <altivec.h>

vector signed int
test1 (vector signed int x)
{
  return vec_abs (x);
}

/* scan-assembler stanzas moved to fold-vec-abs-int-fwrapv.p*.c tests.  */

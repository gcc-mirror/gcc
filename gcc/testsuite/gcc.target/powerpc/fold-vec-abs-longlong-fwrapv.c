/* Verify that overloaded built-ins for vec_abs with long long
   inputs produce the right results.  */

/* { dg-do compile } */
/* { dg-options "-mvsx -O2 -fwrapv" } */
/* { dg-additional-options "-mdejagnu-cpu=power8" { target { ! has_arch_pwr8 } } } */
/* { dg-require-effective-target powerpc_vsx } */

#include <altivec.h>

vector signed long long
test3 (vector signed long long x)
{
  return vec_abs (x);
}

/* scan-assembler stanzas moved to fold-vec-abs-longlong.p*.c.  */


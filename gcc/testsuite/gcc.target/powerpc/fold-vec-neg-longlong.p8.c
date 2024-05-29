/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-options "-mdejagnu-cpu=power8 -mvsx -O2" } */
/* { dg-require-effective-target powerpc_vsx } */


#include "fold-vec-neg-longlong.h"

/* { dg-final { scan-assembler-times "xxspltib|vspltisw" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 0 } } */

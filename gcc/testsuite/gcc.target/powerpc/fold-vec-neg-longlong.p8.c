/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p8vector_ok } */
/* { dg-options "-mpower8-vector -mdejagnu-cpu=power8 -O2" } */


#include "fold-vec-neg-longlong.h"

/* { dg-final { scan-assembler-times "xxspltib|vspltisw" 1 } } */
/* { dg-final { scan-assembler-times "vsubudm" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 0 } } */

/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx_ok } */
/* { dg-options "-mdejagnu-cpu=power9 -mvsx -O2" } */

#include "fold-vec-neg-longlong.h"


/* { dg-final { scan-assembler-times "vnegd" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 0 } } */


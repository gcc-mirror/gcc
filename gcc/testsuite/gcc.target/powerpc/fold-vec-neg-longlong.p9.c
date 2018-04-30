/* Verify that overloaded built-ins for vec_neg with long long
   inputs produce the right code.  */

/* { dg-do compile } */
/* { dg-require-effective-target powerpc_p9vector_ok } */
/* { dg-options "-mcpu=power9 -O2" } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=power9" } } */

#include "fold-vec-neg-longlong.h"


/* { dg-final { scan-assembler-times "vnegd" 1 } } */
/* { dg-final { scan-assembler-times "vmaxsd" 0 } } */


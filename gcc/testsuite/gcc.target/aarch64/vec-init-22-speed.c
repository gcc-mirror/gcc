/* { dg-do compile } */
/* { dg-options "-O3" } */

/* Verify that we recursively generate code for even and odd halves
   instead of fallback code. This is so despite the longer code-gen
   because it has fewer dependencies and thus has lesser cost.  */

#include "vec-init-22.h"

/* { dg-final { scan-assembler-times {\tfmov\td[0-9]+, x[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {\tins\tv[0-9]+\.h\[[1-3]\], w[0-9]+} 6 } } */
/* { dg-final { scan-assembler {\tzip1\tv[0-9]+\.8h, v[0-9]+\.8h, v[0-9]+\.8h} } } */

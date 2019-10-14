/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -ffinite-math-only -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (SIGNALING_GT);

/* We can use non-signaling vector comparison instructions with
   -ffinite-math-only.  */
/* { dg-final { scan-assembler {\n\tvfchdb\t} } } */

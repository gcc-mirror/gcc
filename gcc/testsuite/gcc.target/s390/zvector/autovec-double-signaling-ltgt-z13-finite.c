/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -ffinite-math-only -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (SIGNALING_LTGT);

/* ltgt is the same as eq with -ffinite-math-only.  */
/* { dg-final { scan-assembler {\n\tvfcedb\t} } } */

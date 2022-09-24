/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch -fno-unroll-loops" } */

#include "autovec.h"

AUTOVEC_FLOAT (SIGNALING_LTGT);

/* { dg-final { scan-assembler-times {\n\tvfkhsb\t} 2 } } */
/* { dg-final { scan-assembler {\n\tvo\t} } } */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_FLOAT (QUIET_UNORDERED);

/* { dg-final { scan-assembler {\n\tvfchesb\t} } } */
/* { dg-final { scan-assembler {\n\tvfchsb\t} } } */
/* combine prefers to reorder vsel args instead of using vno.  */
/* { dg-final { scan-assembler {\n\tvo\t} } } */

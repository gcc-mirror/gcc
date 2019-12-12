/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_FLOAT (QUIET_EQ);

/* { dg-final { scan-assembler {\n\tvfcesb\t} } } */

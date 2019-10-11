/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (SIGNALING_LT);

/* { dg-final { scan-assembler {\n\tvfkhdb\t} } } */

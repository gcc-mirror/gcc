/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (QUIET_LE);

/* { dg-final { scan-assembler {\n\tvfchedb\t} } } */

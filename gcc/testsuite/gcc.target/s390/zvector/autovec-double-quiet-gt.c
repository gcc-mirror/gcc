/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (QUIET_GT);

/* { dg-final { scan-assembler {\n\tvfchdb\t} } } */

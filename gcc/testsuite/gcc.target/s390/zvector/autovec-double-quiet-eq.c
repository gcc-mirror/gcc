/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (QUIET_EQ);

/* { dg-final { scan-assembler {\n\tvfcedb\t} } } */

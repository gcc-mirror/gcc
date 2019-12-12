/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (QUIET_ORDERED);

/* { dg-final { scan-assembler {\n\tvfchedb\t} } } */
/* { dg-final { scan-assembler {\n\tvfchdb\t} } } */
/* { dg-final { scan-assembler {\n\tvo\t} } } */

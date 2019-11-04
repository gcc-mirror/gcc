/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (QUIET_UNORDERED);

/* { dg-final { scan-assembler {\n\tvfchedb\t} } } */
/* { dg-final { scan-assembler {\n\tvfchdb\t} } } */
/* combine prefers to reorder vsel args instead of using vno.  */
/* { dg-final { scan-assembler {\n\tvo\t} } } */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch -fno-unroll-loops" } */

#include "autovec.h"

AUTOVEC_FLOAT (QUIET_UNEQ);

/* { dg-final { scan-assembler {\n\tvzero\t} } } */
/* { dg-final { scan-assembler {\n\tvgmf\t} } } */
/* { dg-final { scan-assembler-times {\n\tvfchsb\t} 2 } } */
/* { dg-final { scan-assembler {\n\tvo\t} } } */
/* { dg-final { scan-assembler {\n\tvsel\t} } } */
/* { dg-final { scan-assembler-not {\n\tvx\t} } } */

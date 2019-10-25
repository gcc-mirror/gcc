/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_FLOAT (QUIET_UNEQ);

/* { dg-final { scan-assembler-times {\n\tvfchsb\t} 2 } } */
/* { dg-final { scan-assembler {\n\tvo\t} } } */
/* { dg-final { scan-assembler {\n\tvx\t} } } */

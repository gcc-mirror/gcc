/* { dg-do compile } */
/* { dg-options "-O3 -march=z13 -mzvector -mzarch" } */

#include "autovec.h"

AUTOVEC_DOUBLE (SIGNALING_GE);

/* z13 does not have signaling vector comparison instructions.  */
/* { dg-final { scan-assembler {\n\tkdb\t} } } */

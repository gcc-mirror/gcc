/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch -fexceptions -fnon-call-exceptions -fno-delete-dead-exceptions" } */

#include "autovec.h"

AUTOVEC_FLOAT (SIGNALING_EQ);

/* The vectorizer produces <= and ==, which rtl passes cannot turn into vfkesb
   yet.  */
/* { dg-final { scan-assembler {\n\tvfcesb\t} } } */
/* { dg-final { scan-assembler {\n\tvfkhesb\t} } } */

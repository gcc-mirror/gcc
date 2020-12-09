/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzvector -mzarch -fexceptions -fnon-call-exceptions" } */

#include "autovec.h"

AUTOVEC_DOUBLE (SIGNALING_EQ);

/* The vectorizer produces <= and ==, which rtl passes cannot turn into vfkedb
   yet.  */
/* { dg-final { scan-assembler {\n\tvfcedb\t} } } */
/* { dg-final { scan-assembler {\n\tvfkhedb\t} } } */

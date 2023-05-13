/* { dg-do compile } */
/* { dg-options "-Os" } */

/* Verify that fallback code-sequence is chosen over
   recursively generated code-sequence merged with zip1.  */

#include "vec-init-22.h"

/* { dg-final { scan-assembler {\tfmov\ts[0-9]+, w0|w7} } } */
/* { dg-final { scan-assembler-times {\tins\tv[0-9]+\.h\[[1-7]\], w[0-9]+} 7 } } */

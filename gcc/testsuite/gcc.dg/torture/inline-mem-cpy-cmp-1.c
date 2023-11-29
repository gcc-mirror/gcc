/* { dg-do run } */
/* { dg-options "-finline-stringops -save-temps -g0 -fno-lto" } */
/* { dg-require-effective-target ptr32plus } */
/* { dg-timeout-factor 2 } */

#include "../memcmp-1.c"
/* Yeah, this memcmp test exercises plenty of memcpy, more than any of the
   memcpy tests.  */

/* { dg-final { scan-assembler-not {\mmemcpy\M} } } */
/* { dg-final { scan-assembler-not {(^|\*)\mmemcmp\M} } } */

/* { dg-do run } */
/* { dg-options "-finline-stringops=memcpy -save-temps -g0 -fno-lto" } */

#include "../memcmp-1.c"
/* Yeah, this memcmp test exercises plenty of memcpy, more than any of the
   memcpy tests.  */

/* { dg-final { scan-assembler-not {\mmemcpy\M} } } */

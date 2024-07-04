/* { dg-do run } */
/* { dg-options "-finline-stringops=memcpy -save-temps -g0 -fno-lto" } */
/* { dg-additional-options "-DRUN_FRACTION=11" { target simulator } } */
/* { dg-timeout-factor 2 } */

#include "../memcmp-1.c"
/* Yeah, this memcmp test exercises plenty of memcpy, more than any of the
   memcpy tests.  */

/* { dg-final { scan-assembler-not {\mmemcpy\M} } } */

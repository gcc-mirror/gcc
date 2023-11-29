/* { dg-do run } */
/* { dg-options "-finline-stringops=memcmp -save-temps -g0 -fno-lto" } */

#include "../memcmp-1.c"

/* Check that no memcmp calls remain, but allow for lib_memcmp calls.  */
/* { dg-final { scan-assembler-not {(^|\*)\mmemcmp\M} } } */

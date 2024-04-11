/* { dg-do run } */
/* { dg-options "-march=rv32gc -save-temps -g0 -fno-lto" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -save-temps -g0 -fno-lto" { target { rv64 } } } */
/* { dg-additional-options "-DRUN_FRACTION=11" { target simulator } } */
/* { dg-timeout-factor 2 } */

#include "../../gcc.dg/memcmp-1.c"
/* Yeah, this memcmp test exercises plenty of memcpy, more than any of the
   memcpy tests.  */

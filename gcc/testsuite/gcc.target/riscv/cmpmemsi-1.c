/* { dg-do run } */
/* { dg-options "-march=rv32gc_zbb -save-temps -g0 -fno-lto" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_zbb -save-temps -g0 -fno-lto" { target { rv64 } } } */
/* { dg-timeout-factor 2 } */

#include "../../gcc.dg/memcmp-1.c"

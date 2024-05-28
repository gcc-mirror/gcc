/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */
#include "packh32.c"
/* { dg-final { scan-assembler-times "\\spackh\\s" 1 } } */


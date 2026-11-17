/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zba_zbkb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-g" "-flto" "-O0" } } */

#include "pack64.c"

/* { dg-final { scan-assembler-times "\\spack\\s" 2 } } */
/* { dg-final { scan-assembler-not "\\sslli\\s" } } */
/* { dg-final { scan-assembler-not "\\sadd\\.uw\\s" } } */

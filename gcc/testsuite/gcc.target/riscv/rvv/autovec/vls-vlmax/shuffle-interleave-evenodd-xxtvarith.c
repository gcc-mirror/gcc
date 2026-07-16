/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_xxtvarith -mabi=lp64d -mrvv-max-lmul=m8" } */

#include "shuffle-interleave-evenodd.c"

/* { dg-final { scan-assembler-times "xt.vile" 24 } } */
/* { dg-final { scan-assembler-times "xt.vilo" 24 } } */

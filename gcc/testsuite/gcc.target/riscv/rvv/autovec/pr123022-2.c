/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv_zvl512b -mabi=lp64d -mrvv-vector-bits=zvl -fsigned-char" } */

#include "pr123022.c"

/* { dg-final { scan-assembler-not "vset.*zero,1," } } */

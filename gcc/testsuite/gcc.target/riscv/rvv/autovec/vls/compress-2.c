/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "../vls-vlmax/compress-2.c"

/* { dg-final { scan-assembler-times {\tvcompress\.vm} 5 } } */
/* { dg-final { scan-assembler-times {\tvslideup} 2 } } */

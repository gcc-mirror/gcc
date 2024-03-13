/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "../vls-vlmax/compress-1.c"

/* { dg-final { scan-assembler-times {\tvcompress\.vm} 2 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 -mrvv-max-lmul=m8" } */

#include "../vls-vlmax/merge-6.c"

/* { dg-final { scan-assembler-times {\tvmerge.vvm} 5 } } */

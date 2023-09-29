/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64d -O3 --param=riscv-autovec-lmul=m8" } */

#include "../vls-vlmax/compress-5.c"

/* { dg-final { scan-assembler-times {\tvcompress.vm} 11 } } */

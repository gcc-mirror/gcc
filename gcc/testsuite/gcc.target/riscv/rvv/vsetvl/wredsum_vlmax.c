/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b --param=riscv-autovec-preference=fixed-vlmax -O3" } */


#include <stdint.h>

int16_t foo (int8_t *restrict a)
{
    int16_t sum = 0;
    for (int i = 0; i < 8; i += 1)
      sum += a[i];
    return sum;
}

/* { dg-final { scan-assembler-not {\tvsetivli\tzero,16} } } */

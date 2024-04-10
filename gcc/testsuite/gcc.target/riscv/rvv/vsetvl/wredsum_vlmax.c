/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -O3" } */


#include <stdint-gcc.h>

int16_t foo (int8_t *restrict a)
{
    int16_t sum = 0;
    for (int i = 0; i < 8; i += 1)
      sum += a[i];
    return sum;
}

/* { dg-final { scan-assembler-not {\tvsetivli\tzero,16} } } */

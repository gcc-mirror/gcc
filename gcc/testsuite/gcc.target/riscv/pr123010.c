/* { dg-do compile { target rv64 } } */
/* { dg-options "-O3 -march=rv64gc -mabi=lp64" { target rv64 } } */
#include <stdint-gcc.h>

#define N 2

uint32_t mulu(uint32_t a) {
    return a * N;
}

int32_t muls(int32_t a) {
    return a * N;
}

/* { dg-final { scan-assembler-times "slliw\t" 2 { target rv64 } } } */
/* { dg-final { scan-assembler-not "sr\[al\]i\t" } } */


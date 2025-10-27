/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb_zbs -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb_zbs -mabi=ilp32" { target { rv32 } } } */

#include <stdint.h>
void test(int32_t N, int16_t* A, int16_t val) {
    int32_t i, j;
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            A[i * N + j] += val;
        }
    }
}

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"
void bar() __attribute__((riscv_vector_cc));

vint32m1_t foo(vint32m1_t a, vint32m1_t b) {
    register vint32m1_t x asm("v24") = b;
    bar();
    asm ("#xx %0"::"vr"(x) );
    return x;
}

/* { dg-final { scan-assembler-times "vset" 2 } } */

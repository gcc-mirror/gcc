/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d" } */

#include <riscv_vector.h>

size_t foo ()
{
    return __riscv_vsetvlmax_e8m1 ();
}

/* { dg-final { scan-assembler-times {\tvsetvli\t[a-x0-9]+,zero,e8,m1,ta,ma} 1 } } */

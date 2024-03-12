/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void f (void * in, void *out, int32_t x)
{
    vint32m1_t v = __riscv_vle32_v_i32m1 (in, 4);
    vint32m1_t v2 = __riscv_vle32_v_i32m1_tu (v, in, 4);
    vint32m1_t v3 = __riscv_vaadd_vx_i32m1 (v2, 0, x, 4); /* { dg-error {argument 3 of '__riscv_vaadd_vx_i32m1' must be an integer constant expression} } */
    __riscv_vse32_v_i32m1 (out, v3, 4);
}

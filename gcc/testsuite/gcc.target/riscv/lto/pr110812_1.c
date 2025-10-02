/* { dg-options { -flto -march=rv64gcv -mabi=lp64d } } */

#include "riscv_vector.h"
uint8_t *x, *y;
void foo () {
   int vl = __riscv_vsetvl_e8m8 (100);
   vint8m8_t a = __riscv_vle8_v_i8m8 (x, 100);
   __riscv_vse8_v_i8m8 (y, a, 100);
}

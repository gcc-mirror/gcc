/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

#include <riscv_vector.h>

double k[30], l[30];

int main () {
  for (int i = 0; i < 30; ++i) { k[i] = 1; }

  for (size_t m = 0, avl = 30; avl > 0;) {
    size_t s = __riscv_vsetvl_e8mf8(avl);
    vfloat64m1_t q = __riscv_vle64_v_f64m1(&k[m], s);
    q = __riscv_vfneg_v_f64m1(q, s);
    vuint8mf8_t r = __riscv_vsll_vx_u8mf8(__riscv_vid_v_u8mf8(s), 3, s);
    __riscv_vsoxei8(&l[m], r, q, s);
    avl -= s; m += s;
  }
}

/* { dg-final { scan-assembler-times "vsoxei8" 1 } } */

/* { dg-do run */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */

#include <riscv_vector.h>
#include <vector>

int8_t a[5], d[5], c[5], b[5];
int main() {
  for (size_t e = 0, avl = 1; avl > 0;) {
    size_t f = __riscv_vsetvl_e8m1(avl);
    vint8m1_t g = __riscv_vle8_v_i8m1(&a[e], f);
    vint8mf2_t i = __riscv_vle8ff(
        __riscv_vlm_v_b16(std::vector<uint8_t>((f + 7) / 8, 5).data(), f),
        &b[e], &f, f);
    vint8m1_t j = __riscv_vle8_v_i8m1(&c[e], f);
    vint8m1_t k = __riscv_vredxor_tu(g, i, j, f);
    __riscv_vse8_v_i8m1(&d[e], k, f);
    avl -= f;

    if (f != 1 && avl != 0)
      __builtin_abort ();
    break;
  }
}

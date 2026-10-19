/* { dg-do compile } */
/* { dg-options "-O1 -march=rv64gc" } */

#include <riscv_vector.h>
#include <stdint.h>
#include <stddef.h>

int main() {
    uint8_t val = 3, dup[8];
    size_t vl = __riscv_vsetvl_e8m1(8); /* { dg-error "built-in function '__riscv_vsetvl_e8m1' requires the 'v' ISA extension" } */
    vuint8m1_t v = __riscv_vmv_v_x_u8m1(val, vl); /* { dg-error "built-in function '__riscv_vmv_v_x_u8m1' requires the 'v' ISA extension" } */
    return 0;
}

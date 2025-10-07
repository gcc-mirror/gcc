/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-O0 -std=gnu99" } */

#include <stdint.h>
#include <stdio.h>
#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
uint16_t func_24() {
  BS_VEC(uint32_t, 4) zero = {0};
  BS_VEC(uint8_t, 2)
  BS_VAR_1 = __builtin_shufflevector(
      (BS_VEC(uint8_t, 4))5,
      __builtin_convertvector(zero, BS_VEC(uint8_t, 4)), 5, 0);
  return BS_VAR_1[1];
}
int main() {
  printf("%u\n", func_24());
}

/* { dg-output "5" } */

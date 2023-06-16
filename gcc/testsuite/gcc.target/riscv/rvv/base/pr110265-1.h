#include "riscv_vector.h"

vint8m1_t test_vredand_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredand_vs_i8mf4_i8m1(vector, scalar, vl);
}

vuint32m1_t test_vredand_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredand_vs_u32m8_u32m1(vector, scalar, vl);
}

vint8m1_t test_vredmax_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredmax_vs_i8mf4_i8m1(vector, scalar, vl);
}

vint32m1_t test_vredmax_vs_i32m8_i32m1(vint32m8_t vector, vint32m1_t scalar, size_t vl) {
  return __riscv_vredmax_vs_i32m8_i32m1(vector, scalar, vl);
}

vuint8m1_t test_vredmaxu_vs_u8mf4_u8m1(vuint8mf4_t vector, vuint8m1_t scalar, size_t vl) {
  return __riscv_vredmaxu_vs_u8mf4_u8m1(vector, scalar, vl);
}

vuint32m1_t test_vredmaxu_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredmaxu_vs_u32m8_u32m1(vector, scalar, vl);
}

vint8m1_t test_vredmin_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredmin_vs_i8mf4_i8m1(vector, scalar, vl);
}

vint32m1_t test_vredmin_vs_i32m8_i32m1(vint32m8_t vector, vint32m1_t scalar, size_t vl) {
  return __riscv_vredmin_vs_i32m8_i32m1(vector, scalar, vl);
}

vuint8m1_t test_vredminu_vs_u8mf4_u8m1(vuint8mf4_t vector, vuint8m1_t scalar, size_t vl) {
  return __riscv_vredminu_vs_u8mf4_u8m1(vector, scalar, vl);
}

vuint32m1_t test_vredminu_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredminu_vs_u32m8_u32m1(vector, scalar, vl);
}

vint8m1_t test_vredor_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredor_vs_i8mf4_i8m1(vector, scalar, vl);
}

vuint32m1_t test_vredor_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredor_vs_u32m8_u32m1(vector, scalar, vl);
}

vint8m1_t test_vredsum_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredsum_vs_i8mf4_i8m1(vector, scalar, vl);
}

vuint32m1_t test_vredsum_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredsum_vs_u32m8_u32m1(vector, scalar, vl);
}

vint8m1_t test_vredxor_vs_i8mf4_i8m1(vint8mf4_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredxor_vs_i8mf4_i8m1(vector, scalar, vl);
}

vuint32m1_t test_vredxor_vs_u32m8_u32m1(vuint32m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vredxor_vs_u32m8_u32m1(vector, scalar, vl);
}

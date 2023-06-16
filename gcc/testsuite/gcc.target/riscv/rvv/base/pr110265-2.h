#include "riscv_vector.h"

vint8m1_t test_vredand_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredand_vs_i8mf8_i8m1(vector, scalar, vl);
}

vint8m1_t test_vredmax_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredmax_vs_i8mf8_i8m1(vector, scalar, vl);
}

vuint8m1_t test_vredmaxu_vs_u8mf8_u8m1(vuint8mf8_t vector, vuint8m1_t scalar, size_t vl) {
  return __riscv_vredmaxu_vs_u8mf8_u8m1(vector, scalar, vl);
}

vint8m1_t test_vredmin_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredmin_vs_i8mf8_i8m1(vector, scalar, vl);
}

vuint8m1_t test_vredminu_vs_u8mf8_u8m1(vuint8mf8_t vector, vuint8m1_t scalar, size_t vl) {
  return __riscv_vredminu_vs_u8mf8_u8m1(vector, scalar, vl);
}

vint8m1_t test_vredor_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredor_vs_i8mf8_i8m1(vector, scalar, vl);
}

vint8m1_t test_vredsum_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredsum_vs_i8mf8_i8m1(vector, scalar, vl);
}

vint8m1_t test_vredxor_vs_i8mf8_i8m1(vint8mf8_t vector, vint8m1_t scalar, size_t vl) {
  return __riscv_vredxor_vs_i8mf8_i8m1(vector, scalar, vl);
}

vuint64m1_t test_vredand_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredand_vs_u64m8_u64m1(vector, scalar, vl);
}

vuint64m1_t test_vredmaxu_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredmaxu_vs_u64m8_u64m1(vector, scalar, vl);
}

vuint64m1_t test_vredminu_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredminu_vs_u64m8_u64m1(vector, scalar, vl);
}

vuint64m1_t test_vredor_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredor_vs_u64m8_u64m1(vector, scalar, vl);
}

vuint64m1_t test_vredsum_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredsum_vs_u64m8_u64m1(vector, scalar, vl);
}

vuint64m1_t test_vredxor_vs_u64m8_u64m1(vuint64m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vredxor_vs_u64m8_u64m1(vector, scalar, vl);
}

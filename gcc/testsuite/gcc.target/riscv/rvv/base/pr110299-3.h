#include "riscv_vector.h"

vint16m1_t test_vwredsum_vs_i8mf4_i16m1(vint8mf4_t vector, vint16m1_t scalar, size_t vl) {
  return __riscv_vwredsum_vs_i8mf4_i16m1(vector, scalar, vl);
}

vint32m1_t test_vwredsum_vs_i16m8_i32m1(vint16m8_t vector, vint32m1_t scalar, size_t vl) {
  return __riscv_vwredsum_vs_i16m8_i32m1(vector, scalar, vl);
}

vuint16m1_t test_vwredsumu_vs_u8mf4_u16m1(vuint8mf4_t vector, vuint16m1_t scalar, size_t vl) {
  return __riscv_vwredsumu_vs_u8mf4_u16m1(vector, scalar, vl);
}

vuint32m1_t test_vwredsumu_vs_u16m8_u32m1(vuint16m8_t vector, vuint32m1_t scalar, size_t vl) {
  return __riscv_vwredsumu_vs_u16m8_u32m1(vector, scalar, vl);
}

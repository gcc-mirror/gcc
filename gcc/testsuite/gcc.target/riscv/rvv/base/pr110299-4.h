#include "riscv_vector.h"

vint16m1_t test_vwredsum_vs_i8mf8_i16m1(vint8mf8_t vector, vint16m1_t scalar, size_t vl) {
  return __riscv_vwredsum_vs_i8mf8_i16m1(vector, scalar, vl);
}

vint64m1_t test_vwredsum_vs_i32m8_i64m1(vint32m8_t vector, vint64m1_t scalar, size_t vl) {
  return __riscv_vwredsum_vs_i32m8_i64m1(vector, scalar, vl);
}

vuint16m1_t test_vwredsumu_vs_u8mf8_u16m1(vuint8mf8_t vector, vuint16m1_t scalar, size_t vl) {
  return __riscv_vwredsumu_vs_u8mf8_u16m1(vector, scalar, vl);
}

vuint64m1_t test_vwredsumu_vs_u32m8_u64m1(vuint32m8_t vector, vuint64m1_t scalar, size_t vl) {
  return __riscv_vwredsumu_vs_u32m8_u64m1(vector, scalar, vl);
}

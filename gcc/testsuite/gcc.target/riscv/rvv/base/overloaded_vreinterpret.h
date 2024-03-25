#include "riscv_vector.h"

vuint16m1_t test_vreinterpret_v_b2_u16m1(vbool2_t src) {
  return __riscv_vreinterpret_u16m1(src);
}

vbool4_t test_vreinterpret_v_i32m1_b4(vint32m1_t src) {
  return __riscv_vreinterpret_b4(src);
}

vint8mf2_t test_vreinterpret_v_i16mf2_i8mf2(vint16mf2_t src) {
  return __riscv_vreinterpret_i8mf2(src);
}

vint32mf2_t test_vreinterpret_v_i16mf2_i32mf2(vint16mf2_t src) {
  return __riscv_vreinterpret_i32mf2(src);
}

vint32m1_t test_vreinterpret_v_i16m1_i32m1(vint16m1_t src) {
  return __riscv_vreinterpret_i32m1(src);
}

vint8m4_t test_vreinterpret_v_i32m4_i8m4(vint32m4_t src) {
  return __riscv_vreinterpret_i8m4(src);
}

vuint8m8_t test_vreinterpret_v_u32m8_u8m8(vuint32m8_t src) {
  return __riscv_vreinterpret_u8m8(src);
}

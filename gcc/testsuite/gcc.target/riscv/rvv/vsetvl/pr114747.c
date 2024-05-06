/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -fno-tree-vectorize -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

typedef unsigned short char16_t;

size_t convert_latin1_to_utf16le(const char *src, size_t len, char16_t *dst) {
  char16_t *beg = dst;
  for (size_t vl; len > 0; len -= vl, src += vl, dst += vl) {
    vl = __riscv_vsetvl_e8m4(len);
    vuint8m4_t v = __riscv_vle8_v_u8m4((uint8_t*)src, vl);
    __riscv_vse16_v_u16m8((uint16_t*)dst, __riscv_vzext_vf2_u16m8(v, vl), vl);
  }
  return dst - beg;
}

/* { dg-final { scan-assembler {vsetvli\s+[a-z0-9]+,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]} } } */

/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -ftree-vectorize -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

vbool8_t test () {
  uint8_t arr[32] = {
    1, 2, 7, 1, 3, 4, 5, 3,
    1, 0, 1, 2, 4, 4, 9, 9,
    1, 2, 7, 1, 3, 4, 5, 3,
    1, 0, 1, 2, 4, 4, 9, 9,
  };

  vuint8m1_t varr = __riscv_vle8_v_u8m1(arr, 32);
  vuint8m1_t vand_m = __riscv_vand_vx_u8m1(varr, 1, 32);

  return __riscv_vreinterpret_v_u8m1_b8(vand_m);
}

/* { dg-final { scan-assembler-not {vle[0-9]+\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */
/* { dg-final { scan-assembler-not {vs[0-9]+r\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */

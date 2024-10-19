/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -ftree-vectorize -mrvv-vector-bits=zvl -mrvv-max-lmul=m8" } */

#include "riscv_vector.h"

vuint8m2_t test () {
  uint8_t arr[32] = {
    1, 2, 7, 1, 3, 4, 5, 3,
    1, 0, 1, 2, 4, 4, 9, 9,
    1, 2, 7, 1, 3, 4, 5, 3,
    1, 0, 1, 2, 4, 4, 9, 9,
  };

  return __riscv_vle8_v_u8m2(arr, 4);
}

/* { dg-final { scan-assembler-not {vle[0-9]+\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */
/* { dg-final { scan-assembler-not {vs[09]+r\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */

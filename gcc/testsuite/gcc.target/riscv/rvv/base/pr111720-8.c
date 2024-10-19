/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -ftree-vectorize -mrvv-vector-bits=zvl -mrvv-max-lmul=m8" } */

#include "riscv_vector.h"

vfloat32m1_t test () {
  float arr[32] = {
    1.0, 2.2, 7.8, 1.2, 3.3, 4.7, 5.5, 3.3,
    1.0, 0.2, 1.8, 2.2, 4.3, 4.7, 9.5, 9.3,
    1.0, 2.2, 7.8, 1.2, 3.3, 4.7, 5.5, 3.3,
    1.0, 0.2, 1.8, 2.2, 4.3, 4.7, 9.5, 9.3,
  };

  return __riscv_vle32_v_f32m1(arr, 32);
}

/* { dg-final { scan-assembler-not {vle[0-9]+\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */
/* { dg-final { scan-assembler-not {vs[0-9]+r\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} } } */

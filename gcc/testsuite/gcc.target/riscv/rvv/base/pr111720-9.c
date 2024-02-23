/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d -ftree-vectorize -mrvv-vector-bits=zvl" } */

#include "riscv_vector.h"

vfloat64m8_t test () {
  double arr[8] = {
    1.0, 2.2, 7.8, 1.2, 3.3, 4.7, 5.5, 3.3,
  };

  return __riscv_vle64_v_f64m8(arr, 4);
}

/* { dg-final { scan-assembler-times {vle[0-9]+\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} 1 } } */
/* { dg-final { scan-assembler-times {vs[0-9]+r\.v\s+v[0-9]+,\s*[0-9]+\(sp\)} 1 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

#include "riscv_vector.h"

typedef int fixed_vint32m1_t __attribute__((riscv_rvv_vector_bits(128))); /* { dg-error "'riscv_rvv_vector_bits' applied to non-RVV type 'int'" } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef vint32m1_t fixed_vint32m1_t __attribute__((riscv_rvv_vector_bits(128))); /* { dg-error "'riscv_rvv_vector_bits' is only supported when '-mrvv-vector-bits=zvl' is specified" } */

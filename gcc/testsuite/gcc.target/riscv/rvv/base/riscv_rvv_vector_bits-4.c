/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

#include "riscv_vector.h"

typedef vint32m1_t fixed_vint32m1_t __attribute__((riscv_rvv_vector_bits(128))); /* { dg-error "invalid RVV vector size '128', expected size is '256' based on LMUL of type and '-mrvv-vector-bits=zvl'" } */

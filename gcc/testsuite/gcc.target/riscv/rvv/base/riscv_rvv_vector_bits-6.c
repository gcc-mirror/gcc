/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64 -mrvv-vector-bits=zvl -O3" } */

#include "riscv_vector.h"

typedef vint32mf2_t fixed_vint32mf2_t __attribute__((riscv_rvv_vector_bits(256))); /* { dg-error "invalid RVV vector size '256', expected size is '128' based on LMUL of type and '-mrvv-vector-bits=zvl'" } */

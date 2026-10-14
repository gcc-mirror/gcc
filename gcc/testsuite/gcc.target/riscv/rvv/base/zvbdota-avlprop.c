/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfbdota32f_zvl256b -mabi=lp64d -mrvv-vector-bits=zvl -O2 -fdump-rtl-avlprop-details" } */

#include "riscv_vector.h"

typedef vfloat32m1_t fixed_f32m1
  __attribute__ ((riscv_rvv_vector_bits (256)));

/* The dot product reads all eight accumulator elements even with VL=1.  */
vfloat32m1_t
test_acc_producer (fixed_f32m1 a, fixed_f32m1 b, vfloat32m8_t group,
		   vfloat32m1_t source)
{
  fixed_f32m1 acc = a + b;
  return __riscv_vfbdota_vv_f32m1 (acc, group, source, 0, 1);
}

/* { dg-final { scan-assembler {vfadd\.vv} } } */
/* { dg-final { scan-assembler {vfbdota\.vv} } } */
/* { dg-final { scan-assembler {vsetivli\s+zero,8,e32,m1,} } } */
/* { dg-final { scan-rtl-dump-not {Propagating AVL: \(const_int 1 } "avlprop" } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

vuint8mf2_t
test (vuint16m1_t val, size_t shift, size_t vl)
{
#if __riscv_v_intrinsic == 11000
  #warning "RVV Intrinsics v0.11"
  return __riscv_vnclipu (val, shift, vl);
#endif

#if __riscv_v_intrinsic == 12000
  #warning "RVV Intrinsics v0.12" /* { dg-warning "RVV Intrinsics v0.12" } */
  return __riscv_vnclipu (val, shift, 0, vl);
#endif
}


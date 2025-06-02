/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -O0" } */

/* Use -O0 as otherwise the unused intrinsics get
   optimized away.  We used to ICE here instead of
   emitting an error.  */

#include "riscv_vector.h"

void
clean_subreg (int32_t *in, int32_t *out, size_t m) /* { dg-error {this operation requires the RVV ISA extension} } */
{
  vint16m8_t v24, v8, v16;
  vint32m8_t result = __riscv_vle32_v_i32m8 (in, 32); /* { dg-error {built-in function '__riscv_vle32_v_i32m8\(in, 32\)' requires the 'v' ISA extension} } */
  vint32m1_t v0 = __riscv_vget_v_i32m8_i32m1 (result, 0);
}

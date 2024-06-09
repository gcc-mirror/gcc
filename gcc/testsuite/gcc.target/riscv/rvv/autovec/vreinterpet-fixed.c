/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=zvl -O3" } */

#include "riscv_vector.h"

void test_vreinterpret_v_b64_i8m1 (uint8_t *in, int8_t *out)
{
  vbool64_t vmask = __riscv_vlm_v_b64 (in, 2);
  vint8m1_t vout = __riscv_vreinterpret_v_b64_i8m1 (vmask);
  __riscv_vse8_v_i8m1(out, vout, 16);
}

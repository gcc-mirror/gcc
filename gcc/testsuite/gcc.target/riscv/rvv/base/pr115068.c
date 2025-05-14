/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-add-options riscv_v } */
/* { dg-additional-options "-std=gnu99 -Wno-pedantic" } */

#include <stdint.h>
#include "riscv_vector.h"

vfloat64m8_t
test_vfwadd_wf_f64m8_m (vbool8_t vm, vfloat64m8_t vs2, float rs1, size_t vl)
{
  return __riscv_vfwadd_wf_f64m8_m (vm, vs2, rs1, vl);
}

vint64m8_t
test_vwadd_wx_i64m8_m (vbool8_t vm, vint64m8_t vs2, int32_t rs1, size_t vl)
{
  return __riscv_vwadd_wx_i64m8_m (vm, vs2, rs1, vl);
}

vint64m8_t
test_vwsub_wx_i64m8_m (vbool8_t vm, vint64m8_t vs2, int32_t rs1, size_t vl)
{
  return __riscv_vwsub_wx_i64m8_m (vm, vs2, rs1, vl);
}

char global_memory[1024];
void *fake_memory = (void *) global_memory;

int
main ()
{
  asm volatile ("fence" ::: "memory");
  vfloat64m8_t vfwadd_wf_f64m8_m_vd = test_vfwadd_wf_f64m8_m (
    __riscv_vreinterpret_v_i8m1_b8 (__riscv_vundefined_i8m1 ()),
    __riscv_vundefined_f64m8 (), 1.0, __riscv_vsetvlmax_e64m8 ());
  asm volatile ("" ::"vr"(vfwadd_wf_f64m8_m_vd) : "memory");

  asm volatile ("fence" ::: "memory");
  vint64m8_t vwadd_wx_i64m8_m_vd = test_vwadd_wx_i64m8_m (
    __riscv_vreinterpret_v_i8m1_b8 (__riscv_vundefined_i8m1 ()),
    __riscv_vundefined_i64m8 (), 1.0, __riscv_vsetvlmax_e64m8 ());
  asm volatile ("" ::"vr"(vwadd_wx_i64m8_m_vd) : "memory");

  asm volatile ("fence" ::: "memory");
  vint64m8_t vwsub_wx_i64m8_m_vd = test_vwsub_wx_i64m8_m (
    __riscv_vreinterpret_v_i8m1_b8 (__riscv_vundefined_i8m1 ()),
    __riscv_vundefined_i64m8 (), 1.0, __riscv_vsetvlmax_e64m8 ());
  asm volatile ("" ::"vr"(vwsub_wx_i64m8_m_vd) : "memory");

  return 0;
}

/* { dg-final { scan-assembler-not "vfwadd.wf\tv0.*v0" } } */
/* { dg-final { scan-assembler-not "vwadd.wx\tv0.*v0" } } */
/* { dg-final { scan-assembler-not "vwsub.wx\tv0.*v0" } } */

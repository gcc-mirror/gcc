/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O0" } */

#include "riscv_vector.h"

int
f0 ()
{
  float b;
  vfloat32m1_t c;
  vbool32_t d = __riscv_vmflt_vf_f32m1_b32 (c, b, 8);
  return 0;
}

int
f1 ()
{
  vfloat32m1_t c;
  vbool32_t d = __riscv_vmflt_vf_f32m1_b32 (c, 0, 8);
  return 0;
}

int
f2 ()
{
  vfloat32m1_t c;
  vbool32_t d = __riscv_vmflt_vf_f32m1_b32 (c, 55.55, 8);
  return 0;
}

int
f3 ()
{
  int32_t b;
  vint32m1_t c;
  vbool32_t d = __riscv_vmseq_vx_i32m1_b32 (c, b, 8);
  return 0;
}

int
f4 ()
{
  vint32m1_t c;
  vbool32_t d = __riscv_vmseq_vx_i32m1_b32 (c, 11, 8);
  return 0;
}

int
f5 ()
{
  int64_t b;
  vint64m1_t c;
  vbool64_t d = __riscv_vmseq_vx_i64m1_b64 (c, b, 8);
  return 0;
}

int
f6 ()
{
  vint64m1_t c;
  vbool64_t d = __riscv_vmseq_vx_i64m1_b64 (c, 11, 8);
  return 0;
}

int
f7 ()
{
  vint64m1_t c;
  vbool64_t d = __riscv_vmseq_vx_i64m1_b64 (c, 0xAAAA, 8);
  return 0;
}

int
f8 ()
{
  vint64m1_t c;
  vbool64_t d = __riscv_vmseq_vx_i64m1_b64 (c, 0xAAAAAAAAAAAAAA, 8);
  return 0;
}

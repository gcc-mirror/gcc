/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vfloat32mf2_t test___riscv_vmerge_vvm_f32mf2(vfloat32mf2_t op1,vfloat32mf2_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32mf2(op1,op2,selector,31);
}


vfloat32m1_t test___riscv_vmerge_vvm_f32m1(vfloat32m1_t op1,vfloat32m1_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m1(op1,op2,selector,31);
}


vfloat32m2_t test___riscv_vmerge_vvm_f32m2(vfloat32m2_t op1,vfloat32m2_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m2(op1,op2,selector,31);
}


vfloat32m4_t test___riscv_vmerge_vvm_f32m4(vfloat32m4_t op1,vfloat32m4_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m4(op1,op2,selector,31);
}


vfloat32m8_t test___riscv_vmerge_vvm_f32m8(vfloat32m8_t op1,vfloat32m8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m8(op1,op2,selector,31);
}


vfloat64m1_t test___riscv_vmerge_vvm_f64m1(vfloat64m1_t op1,vfloat64m1_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m1(op1,op2,selector,31);
}


vfloat64m2_t test___riscv_vmerge_vvm_f64m2(vfloat64m2_t op1,vfloat64m2_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m2(op1,op2,selector,31);
}


vfloat64m4_t test___riscv_vmerge_vvm_f64m4(vfloat64m4_t op1,vfloat64m4_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m4(op1,op2,selector,31);
}


vfloat64m8_t test___riscv_vmerge_vvm_f64m8(vfloat64m8_t op1,vfloat64m8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m8(op1,op2,selector,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */

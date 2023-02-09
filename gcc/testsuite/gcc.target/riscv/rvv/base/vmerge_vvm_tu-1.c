/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vfloat32mf2_t test___riscv_vmerge_vvm_f32mf2_tu(vfloat32mf2_t merge,vfloat32mf2_t op1,vfloat32mf2_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32mf2_tu(merge,op1,op2,selector,vl);
}


vfloat32m1_t test___riscv_vmerge_vvm_f32m1_tu(vfloat32m1_t merge,vfloat32m1_t op1,vfloat32m1_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m1_tu(merge,op1,op2,selector,vl);
}


vfloat32m2_t test___riscv_vmerge_vvm_f32m2_tu(vfloat32m2_t merge,vfloat32m2_t op1,vfloat32m2_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m2_tu(merge,op1,op2,selector,vl);
}


vfloat32m4_t test___riscv_vmerge_vvm_f32m4_tu(vfloat32m4_t merge,vfloat32m4_t op1,vfloat32m4_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m4_tu(merge,op1,op2,selector,vl);
}


vfloat32m8_t test___riscv_vmerge_vvm_f32m8_tu(vfloat32m8_t merge,vfloat32m8_t op1,vfloat32m8_t op2,vbool4_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f32m8_tu(merge,op1,op2,selector,vl);
}


vfloat64m1_t test___riscv_vmerge_vvm_f64m1_tu(vfloat64m1_t merge,vfloat64m1_t op1,vfloat64m1_t op2,vbool64_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m1_tu(merge,op1,op2,selector,vl);
}


vfloat64m2_t test___riscv_vmerge_vvm_f64m2_tu(vfloat64m2_t merge,vfloat64m2_t op1,vfloat64m2_t op2,vbool32_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m2_tu(merge,op1,op2,selector,vl);
}


vfloat64m4_t test___riscv_vmerge_vvm_f64m4_tu(vfloat64m4_t merge,vfloat64m4_t op1,vfloat64m4_t op2,vbool16_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m4_tu(merge,op1,op2,selector,vl);
}


vfloat64m8_t test___riscv_vmerge_vvm_f64m8_tu(vfloat64m8_t merge,vfloat64m8_t op1,vfloat64m8_t op2,vbool8_t selector,size_t vl)
{
    return __riscv_vmerge_vvm_f64m8_tu(merge,op1,op2,selector,vl);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vmerge\.vvm\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vbool64_t test___riscv_vmsleu(vbool64_t mask,vuint8mf8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool32_t test___riscv_vmsleu(vbool32_t mask,vuint8mf4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool16_t test___riscv_vmsleu(vbool16_t mask,vuint8mf2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool8_t test___riscv_vmsleu(vbool8_t mask,vuint8m1_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool4_t test___riscv_vmsleu(vbool4_t mask,vuint8m2_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool2_t test___riscv_vmsleu(vbool2_t mask,vuint8m4_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool1_t test___riscv_vmsleu(vbool1_t mask,vuint8m8_t op1,uint8_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool64_t test___riscv_vmsleu(vbool64_t mask,vuint16mf4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool32_t test___riscv_vmsleu(vbool32_t mask,vuint16mf2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool16_t test___riscv_vmsleu(vbool16_t mask,vuint16m1_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool8_t test___riscv_vmsleu(vbool8_t mask,vuint16m2_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool4_t test___riscv_vmsleu(vbool4_t mask,vuint16m4_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool2_t test___riscv_vmsleu(vbool2_t mask,vuint16m8_t op1,uint16_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool64_t test___riscv_vmsleu(vbool64_t mask,vuint32mf2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool32_t test___riscv_vmsleu(vbool32_t mask,vuint32m1_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool16_t test___riscv_vmsleu(vbool16_t mask,vuint32m2_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool8_t test___riscv_vmsleu(vbool8_t mask,vuint32m4_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool4_t test___riscv_vmsleu(vbool4_t mask,vuint32m8_t op1,uint32_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool64_t test___riscv_vmsleu(vbool64_t mask,vuint64m1_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool32_t test___riscv_vmsleu(vbool32_t mask,vuint64m2_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool16_t test___riscv_vmsleu(vbool16_t mask,vuint64m4_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}


vbool8_t test___riscv_vmsleu(vbool8_t mask,vuint64m8_t op1,uint64_t op2,size_t vl)
{
    return __riscv_vmsleu(op1,op2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*t[au],\s*m[au]\s+vmsleu\.vx\s+v[0-9]+,\s*v[0-9]+,\s*[a-x0-9]+\s+} 1 } } */

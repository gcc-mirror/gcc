/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmadd_tum(vbool64_t mask,vint8mf8_t vd,int8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8mf4_t test___riscv_vmadd_tum(vbool32_t mask,vint8mf4_t vd,int8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8mf2_t test___riscv_vmadd_tum(vbool16_t mask,vint8mf2_t vd,int8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8m1_t test___riscv_vmadd_tum(vbool8_t mask,vint8m1_t vd,int8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8m2_t test___riscv_vmadd_tum(vbool4_t mask,vint8m2_t vd,int8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8m4_t test___riscv_vmadd_tum(vbool2_t mask,vint8m4_t vd,int8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint8m8_t test___riscv_vmadd_tum(vbool1_t mask,vint8m8_t vd,int8_t rs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16mf4_t test___riscv_vmadd_tum(vbool64_t mask,vint16mf4_t vd,int16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16mf2_t test___riscv_vmadd_tum(vbool32_t mask,vint16mf2_t vd,int16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16m1_t test___riscv_vmadd_tum(vbool16_t mask,vint16m1_t vd,int16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16m2_t test___riscv_vmadd_tum(vbool8_t mask,vint16m2_t vd,int16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16m4_t test___riscv_vmadd_tum(vbool4_t mask,vint16m4_t vd,int16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint16m8_t test___riscv_vmadd_tum(vbool2_t mask,vint16m8_t vd,int16_t rs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint32mf2_t test___riscv_vmadd_tum(vbool64_t mask,vint32mf2_t vd,int32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint32m1_t test___riscv_vmadd_tum(vbool32_t mask,vint32m1_t vd,int32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint32m2_t test___riscv_vmadd_tum(vbool16_t mask,vint32m2_t vd,int32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint32m4_t test___riscv_vmadd_tum(vbool8_t mask,vint32m4_t vd,int32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint32m8_t test___riscv_vmadd_tum(vbool4_t mask,vint32m8_t vd,int32_t rs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint64m1_t test___riscv_vmadd_tum(vbool64_t mask,vint64m1_t vd,int64_t rs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint64m2_t test___riscv_vmadd_tum(vbool32_t mask,vint64m2_t vd,int64_t rs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint64m4_t test___riscv_vmadd_tum(vbool16_t mask,vint64m4_t vd,int64_t rs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vint64m8_t test___riscv_vmadd_tum(vbool8_t mask,vint64m8_t vd,int64_t rs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8mf8_t test___riscv_vmadd_tum(vbool64_t mask,vuint8mf8_t vd,uint8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8mf4_t test___riscv_vmadd_tum(vbool32_t mask,vuint8mf4_t vd,uint8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8mf2_t test___riscv_vmadd_tum(vbool16_t mask,vuint8mf2_t vd,uint8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8m1_t test___riscv_vmadd_tum(vbool8_t mask,vuint8m1_t vd,uint8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8m2_t test___riscv_vmadd_tum(vbool4_t mask,vuint8m2_t vd,uint8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8m4_t test___riscv_vmadd_tum(vbool2_t mask,vuint8m4_t vd,uint8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint8m8_t test___riscv_vmadd_tum(vbool1_t mask,vuint8m8_t vd,uint8_t rs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16mf4_t test___riscv_vmadd_tum(vbool64_t mask,vuint16mf4_t vd,uint16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16mf2_t test___riscv_vmadd_tum(vbool32_t mask,vuint16mf2_t vd,uint16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16m1_t test___riscv_vmadd_tum(vbool16_t mask,vuint16m1_t vd,uint16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16m2_t test___riscv_vmadd_tum(vbool8_t mask,vuint16m2_t vd,uint16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16m4_t test___riscv_vmadd_tum(vbool4_t mask,vuint16m4_t vd,uint16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint16m8_t test___riscv_vmadd_tum(vbool2_t mask,vuint16m8_t vd,uint16_t rs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint32mf2_t test___riscv_vmadd_tum(vbool64_t mask,vuint32mf2_t vd,uint32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint32m1_t test___riscv_vmadd_tum(vbool32_t mask,vuint32m1_t vd,uint32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint32m2_t test___riscv_vmadd_tum(vbool16_t mask,vuint32m2_t vd,uint32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint32m4_t test___riscv_vmadd_tum(vbool8_t mask,vuint32m4_t vd,uint32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint32m8_t test___riscv_vmadd_tum(vbool4_t mask,vuint32m8_t vd,uint32_t rs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint64m1_t test___riscv_vmadd_tum(vbool64_t mask,vuint64m1_t vd,uint64_t rs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint64m2_t test___riscv_vmadd_tum(vbool32_t mask,vuint64m2_t vd,uint64_t rs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint64m4_t test___riscv_vmadd_tum(vbool16_t mask,vuint64m4_t vd,uint64_t rs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}


vuint64m8_t test___riscv_vmadd_tum(vbool8_t mask,vuint64m8_t vd,uint64_t rs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vmadd_tum(mask,vd,rs1,vs2,31);
}



/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf8,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*mf2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m1,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e8,\s*m8,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*mf2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m1,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e16,\s*m8,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*mf2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m1,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e32,\s*m8,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m1,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m2,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m4,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetivli\s+zero,\s*31,\s*e64,\s*m8,\s*tu,\s*m[au]\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */

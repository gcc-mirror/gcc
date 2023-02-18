/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vint8mf8_t test___riscv_vmacc_tumu(vbool64_t mask,vint8mf8_t vd,int8_t rs1,vint8mf8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8mf4_t test___riscv_vmacc_tumu(vbool32_t mask,vint8mf4_t vd,int8_t rs1,vint8mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8mf2_t test___riscv_vmacc_tumu(vbool16_t mask,vint8mf2_t vd,int8_t rs1,vint8mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8m1_t test___riscv_vmacc_tumu(vbool8_t mask,vint8m1_t vd,int8_t rs1,vint8m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8m2_t test___riscv_vmacc_tumu(vbool4_t mask,vint8m2_t vd,int8_t rs1,vint8m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8m4_t test___riscv_vmacc_tumu(vbool2_t mask,vint8m4_t vd,int8_t rs1,vint8m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint8m8_t test___riscv_vmacc_tumu(vbool1_t mask,vint8m8_t vd,int8_t rs1,vint8m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16mf4_t test___riscv_vmacc_tumu(vbool64_t mask,vint16mf4_t vd,int16_t rs1,vint16mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16mf2_t test___riscv_vmacc_tumu(vbool32_t mask,vint16mf2_t vd,int16_t rs1,vint16mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16m1_t test___riscv_vmacc_tumu(vbool16_t mask,vint16m1_t vd,int16_t rs1,vint16m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16m2_t test___riscv_vmacc_tumu(vbool8_t mask,vint16m2_t vd,int16_t rs1,vint16m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16m4_t test___riscv_vmacc_tumu(vbool4_t mask,vint16m4_t vd,int16_t rs1,vint16m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint16m8_t test___riscv_vmacc_tumu(vbool2_t mask,vint16m8_t vd,int16_t rs1,vint16m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint32mf2_t test___riscv_vmacc_tumu(vbool64_t mask,vint32mf2_t vd,int32_t rs1,vint32mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint32m1_t test___riscv_vmacc_tumu(vbool32_t mask,vint32m1_t vd,int32_t rs1,vint32m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint32m2_t test___riscv_vmacc_tumu(vbool16_t mask,vint32m2_t vd,int32_t rs1,vint32m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint32m4_t test___riscv_vmacc_tumu(vbool8_t mask,vint32m4_t vd,int32_t rs1,vint32m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint32m8_t test___riscv_vmacc_tumu(vbool4_t mask,vint32m8_t vd,int32_t rs1,vint32m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint64m1_t test___riscv_vmacc_tumu(vbool64_t mask,vint64m1_t vd,int64_t rs1,vint64m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint64m2_t test___riscv_vmacc_tumu(vbool32_t mask,vint64m2_t vd,int64_t rs1,vint64m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint64m4_t test___riscv_vmacc_tumu(vbool16_t mask,vint64m4_t vd,int64_t rs1,vint64m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vint64m8_t test___riscv_vmacc_tumu(vbool8_t mask,vint64m8_t vd,int64_t rs1,vint64m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8mf8_t test___riscv_vmacc_tumu(vbool64_t mask,vuint8mf8_t vd,uint8_t rs1,vuint8mf8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8mf4_t test___riscv_vmacc_tumu(vbool32_t mask,vuint8mf4_t vd,uint8_t rs1,vuint8mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8mf2_t test___riscv_vmacc_tumu(vbool16_t mask,vuint8mf2_t vd,uint8_t rs1,vuint8mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8m1_t test___riscv_vmacc_tumu(vbool8_t mask,vuint8m1_t vd,uint8_t rs1,vuint8m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8m2_t test___riscv_vmacc_tumu(vbool4_t mask,vuint8m2_t vd,uint8_t rs1,vuint8m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8m4_t test___riscv_vmacc_tumu(vbool2_t mask,vuint8m4_t vd,uint8_t rs1,vuint8m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint8m8_t test___riscv_vmacc_tumu(vbool1_t mask,vuint8m8_t vd,uint8_t rs1,vuint8m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16mf4_t test___riscv_vmacc_tumu(vbool64_t mask,vuint16mf4_t vd,uint16_t rs1,vuint16mf4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16mf2_t test___riscv_vmacc_tumu(vbool32_t mask,vuint16mf2_t vd,uint16_t rs1,vuint16mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16m1_t test___riscv_vmacc_tumu(vbool16_t mask,vuint16m1_t vd,uint16_t rs1,vuint16m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16m2_t test___riscv_vmacc_tumu(vbool8_t mask,vuint16m2_t vd,uint16_t rs1,vuint16m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16m4_t test___riscv_vmacc_tumu(vbool4_t mask,vuint16m4_t vd,uint16_t rs1,vuint16m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint16m8_t test___riscv_vmacc_tumu(vbool2_t mask,vuint16m8_t vd,uint16_t rs1,vuint16m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint32mf2_t test___riscv_vmacc_tumu(vbool64_t mask,vuint32mf2_t vd,uint32_t rs1,vuint32mf2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint32m1_t test___riscv_vmacc_tumu(vbool32_t mask,vuint32m1_t vd,uint32_t rs1,vuint32m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint32m2_t test___riscv_vmacc_tumu(vbool16_t mask,vuint32m2_t vd,uint32_t rs1,vuint32m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint32m4_t test___riscv_vmacc_tumu(vbool8_t mask,vuint32m4_t vd,uint32_t rs1,vuint32m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint32m8_t test___riscv_vmacc_tumu(vbool4_t mask,vuint32m8_t vd,uint32_t rs1,vuint32m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint64m1_t test___riscv_vmacc_tumu(vbool64_t mask,vuint64m1_t vd,uint64_t rs1,vuint64m1_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint64m2_t test___riscv_vmacc_tumu(vbool32_t mask,vuint64m2_t vd,uint64_t rs1,vuint64m2_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint64m4_t test___riscv_vmacc_tumu(vbool16_t mask,vuint64m4_t vd,uint64_t rs1,vuint64m4_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}


vuint64m8_t test___riscv_vmacc_tumu(vbool8_t mask,vuint64m8_t vd,uint64_t rs1,vuint64m8_t vs2,size_t vl)
{
    return __riscv_vmacc_tumu(mask,vd,rs1,vs2,32);
}



/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf8,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf4,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*mf2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m1,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m4,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e8,\s*m8,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*mf2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m1,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m2,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e32,\s*m8,\s*tu,\s*mu\s+vma[c-d][c-d]\.vx\s+v[0-9]+,\s*[a-x0-9]+,\s*v[0-9]+,\s*v0.t\s+} 2 } } */
/* { dg-final { scan-assembler-times {vma[c-d][c-d]\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*v0.t\s+} 8 } } */

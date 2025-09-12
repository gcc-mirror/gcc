/* { dg-do compile } */
/* { dg-options "-march=rv32gv_xandesvdot -O3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_xandesvdot -O3 -mabi=lp64d" { target { rv64 } } } */

#include "andes_vector.h"

vuint32mf2_t
test_vd4dotu_vv_u32mf2 (vuint32mf2_t vd, vuint8mf2_t vs1, vuint8mf2_t vs2,
			size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint32m1_t
test_vd4dotu_vv_u32m1 (vuint32m1_t vd, vuint8m1_t vs1, vuint8m1_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint32m2_t
test_vd4dotu_vv_u32m2 (vuint32m2_t vd, vuint8m2_t vs1, vuint8m2_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint32m4_t
test_vd4dotu_vv_u32m4 (vuint32m4_t vd, vuint8m4_t vs1, vuint8m4_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint32m8_t
test_vd4dotu_vv_u32m8 (vuint32m8_t vd, vuint8m8_t vs1, vuint8m8_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint64m1_t
test_vd4dotu_vv_u64m1 (vuint64m1_t vd, vuint16m1_t vs1, vuint16m1_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint64m2_t
test_vd4dotu_vv_u64m2 (vuint64m2_t vd, vuint16m2_t vs1, vuint16m2_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint64m4_t
test_vd4dotu_vv_u64m4 (vuint64m4_t vd, vuint16m4_t vs1, vuint16m4_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint64m8_t
test_vd4dotu_vv_u64m8 (vuint64m8_t vd, vuint16m8_t vs1, vuint16m8_t vs2,
		       size_t vl)
{
  return __riscv_nds_vd4dotu (vd, vs1, vs2, vl);
}

vuint32mf2_t
test_vd4dotu_vv_u32mf2_m (vbool64_t vm, vuint32mf2_t vd, vuint8mf2_t vs1,
			  vuint8mf2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint32m1_t
test_vd4dotu_vv_u32m1_m (vbool32_t vm, vuint32m1_t vd, vuint8m1_t vs1,
			 vuint8m1_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint32m2_t
test_vd4dotu_vv_u32m2_m (vbool16_t vm, vuint32m2_t vd, vuint8m2_t vs1,
			 vuint8m2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint32m4_t
test_vd4dotu_vv_u32m4_m (vbool8_t vm, vuint32m4_t vd, vuint8m4_t vs1,
			 vuint8m4_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint32m8_t
test_vd4dotu_vv_u32m8_m (vbool4_t vm, vuint32m8_t vd, vuint8m8_t vs1,
			 vuint8m8_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint64m1_t
test_vd4dotu_vv_u64m1_m (vbool64_t vm, vuint64m1_t vd, vuint16m1_t vs1,
			 vuint16m1_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint64m2_t
test_vd4dotu_vv_u64m2_m (vbool32_t vm, vuint64m2_t vd, vuint16m2_t vs1,
			 vuint16m2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint64m4_t
test_vd4dotu_vv_u64m4_m (vbool16_t vm, vuint64m4_t vd, vuint16m4_t vs1,
			 vuint16m4_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}

vuint64m8_t
test_vd4dotu_vv_u64m8_m (vbool8_t vm, vuint64m8_t vd, vuint16m8_t vs1,
			 vuint16m8_t vs2, size_t vl)
{
  return __riscv_nds_vd4dotu (vm, vd, vs1, vs2, vl);
}
/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+nds\.vd4dotu[ivxfswum.]*\s+} 18 } } */

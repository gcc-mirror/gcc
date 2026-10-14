/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwdota8i_zvqwdota16i_zvfwdota16bf_zvfqwdota8f_zvfbfmin -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32m1_t
test_vqwdota_uu_8 (vuint32m1_t vd, vuint8mf4_t vs2,
		   vuint8mf4_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_u8mf4_u8mf4_u32m1 (vd, vs2, vs1, vl);
}

vint32m1_t
test_vqwdota_us_8 (vint32m1_t vd, vuint8mf4_t vs2,
		   vint8mf4_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_u8mf4_i8mf4_i32m1 (vd, vs2, vs1, vl);
}

vint32m1_t
test_vqwdota_su_8 (vint32m1_t vd, vint8mf4_t vs2,
		   vuint8mf4_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_i8mf4_u8mf4_i32m1 (vd, vs2, vs1, vl);
}

vint32m1_t
test_vqwdota_ss_8 (vint32m1_t vd, vint8mf4_t vs2,
		   vint8mf4_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_i8mf4_i8mf4_i32m1 (vd, vs2, vs1, vl);
}

vuint64m1_t
test_vqwdota_uu_16 (vuint64m1_t vd, vuint16mf2_t vs2,
		    vuint16mf2_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_u16mf2_u16mf2_u64m1 (vd, vs2, vs1, vl);
}

vint64m1_t
test_vqwdota_us_16 (vint64m1_t vd, vuint16mf2_t vs2,
		    vint16mf2_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_u16mf2_i16mf2_i64m1 (vd, vs2, vs1, vl);
}

vint64m1_t
test_vqwdota_su_16 (vint64m1_t vd, vint16mf2_t vs2,
		    vuint16mf2_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_i16mf2_u16mf2_i64m1 (vd, vs2, vs1, vl);
}

vint64m1_t
test_vqwdota_ss_16 (vint64m1_t vd, vint16mf2_t vs2,
		    vint16mf2_t vs1, size_t vl)
{
  return __riscv_vqwdota_vv_i16mf2_i16mf2_i64m1 (vd, vs2, vs1, vl);
}

vfloat32m1_t
test_vfqwdota_e4m3_e4m3 (vfloat32m1_t vd, vfloat8e4m3mf4_t vs2,
			 vfloat8e4m3mf4_t vs1, size_t vl)
{
  return __riscv_vfqwdota_vv_f8e4m3mf4_f8e4m3mf4_f32m1 (vd, vs2, vs1, vl);
}

vfloat32m1_t
test_vfqwdota_e4m3_e5m2 (vfloat32m1_t vd, vfloat8e4m3mf4_t vs2,
			 vfloat8e5m2mf4_t vs1, size_t vl)
{
  return __riscv_vfqwdota_vv_f8e4m3mf4_f8e5m2mf4_f32m1 (vd, vs2, vs1, vl);
}

vfloat32m1_t
test_vfqwdota_e5m2_e4m3 (vfloat32m1_t vd, vfloat8e5m2mf4_t vs2,
			 vfloat8e4m3mf4_t vs1, size_t vl)
{
  return __riscv_vfqwdota_vv_f8e5m2mf4_f8e4m3mf4_f32m1 (vd, vs2, vs1, vl);
}

vfloat32m1_t
test_vfqwdota_e5m2_e5m2 (vfloat32m1_t vd, vfloat8e5m2mf4_t vs2,
			 vfloat8e5m2mf4_t vs1, size_t vl)
{
  return __riscv_vfqwdota_vv_f8e5m2mf4_f8e5m2mf4_f32m1 (vd, vs2, vs1, vl);
}

vfloat32m1_t
test_vfwdota_bf16 (vfloat32m1_t vd, vbfloat16mf2_t vs2,
		   vbfloat16mf2_t vs1, size_t vl)
{
  return __riscv_vfwdota_vv_bf16mf2_bf16mf2_f32m1 (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times {vqwdotau\.vv} 4 } } */
/* { dg-final { scan-assembler-times {vqwdotas\.vv} 4 } } */
/* { dg-final { scan-assembler-times {vfqwdota\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfqwdota\.alt\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfwdota\.vv} 1 } } */

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e8,\s*mf4,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e8alt,\s*mf4,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e16,\s*mf2,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e16alt,\s*mf2,\s*ta,\s*ma} 3 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvqwbdota8i_zvqwbdota16i_zvfwbdota16bf_zvfqwbdota8f_zvfbdota32f_zvfbfmin_zvl4096b -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint32m8_t
test_vqwbdota_uu_8 (vuint32m8_t vd, vuint8m8_t vs2,
		    vuint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_u8m1_u32m8 (vd, vs2, vs1, 8, vl);
}

vint32m8_t
test_vqwbdota_us_8 (vint32m8_t vd, vuint8m8_t vs2,
		    vint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u8m8_i8m1_i32m8 (vd, vs2, vs1, 8, vl);
}

vint32m8_t
test_vqwbdota_su_8 (vint32m8_t vd, vint8m8_t vs2,
		    vuint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_i8m8_u8m1_i32m8 (vd, vs2, vs1, 8, vl);
}

vint32m8_t
test_vqwbdota_ss_8 (vint32m8_t vd, vint8m8_t vs2,
		    vint8m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_i8m8_i8m1_i32m8 (vd, vs2, vs1, 8, vl);
}

vuint64m8_t
test_vqwbdota_uu_16 (vuint64m8_t vd, vuint16m8_t vs2,
		     vuint16m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m8 (vd, vs2, vs1, 16, vl);
}

vint64m8_t
test_vqwbdota_us_16 (vint64m8_t vd, vuint16m8_t vs2,
		     vint16m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_i16m1_i64m8 (vd, vs2, vs1, 16, vl);
}

vint64m8_t
test_vqwbdota_su_16 (vint64m8_t vd, vint16m8_t vs2,
		     vuint16m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_i16m8_u16m1_i64m8 (vd, vs2, vs1, 16, vl);
}

vint64m8_t
test_vqwbdota_ss_16 (vint64m8_t vd, vint16m8_t vs2,
		     vint16m1_t vs1, size_t vl)
{
  return __riscv_vqwbdota_vv_i16m8_i16m1_i64m8 (vd, vs2, vs1, 16, vl);
}

vfloat32m8_t
test_vfqwbdota_e4m3_e4m3 (vfloat32m8_t vd, vfloat8e4m3m8_t vs2,
			  vfloat8e4m3m1_t vs1, size_t vl)
{
  return __riscv_vfqwbdota_vv_f8e4m3m8_f8e4m3m1_f32m8 (vd, vs2, vs1, 24, vl);
}

vfloat32m8_t
test_vfqwbdota_e4m3_e5m2 (vfloat32m8_t vd, vfloat8e4m3m8_t vs2,
			  vfloat8e5m2m1_t vs1, size_t vl)
{
  return __riscv_vfqwbdota_vv_f8e4m3m8_f8e5m2m1_f32m8 (vd, vs2, vs1, 24, vl);
}

vfloat32m8_t
test_vfqwbdota_e5m2_e4m3 (vfloat32m8_t vd, vfloat8e5m2m8_t vs2,
			  vfloat8e4m3m1_t vs1, size_t vl)
{
  return __riscv_vfqwbdota_vv_f8e5m2m8_f8e4m3m1_f32m8 (vd, vs2, vs1, 32, vl);
}

vfloat32m8_t
test_vfqwbdota_e5m2_e5m2 (vfloat32m8_t vd, vfloat8e5m2m8_t vs2,
			  vfloat8e5m2m1_t vs1, size_t vl)
{
  return __riscv_vfqwbdota_vv_f8e5m2m8_f8e5m2m1_f32m8 (vd, vs2, vs1, 32, vl);
}

vfloat32m8_t
test_vfwbdota_bf16 (vfloat32m8_t vd, vbfloat16m8_t vs2,
		    vbfloat16m1_t vs1, size_t vl)
{
  return __riscv_vfwbdota_vv_bf16m8_bf16m1_f32m8 (vd, vs2, vs1, 40, vl);
}

vfloat32m8_t
test_vfbdota (vfloat32m8_t vd, vfloat32m8_t vs2,
	      vfloat32m1_t vs1, size_t vl)
{
  return __riscv_vfbdota_vv_f32m8 (vd, vs2, vs1, 48, vl);
}

vfloat32m8_t
test_vfbdota_rm (vfloat32m8_t vd, vfloat32m8_t vs2,
		 vfloat32m1_t vs1, size_t vl)
{
  return __riscv_vfbdota_vv_f32m8_rm (vd, vs2, vs1, 56,
				       __RISCV_FRM_RDN, vl);
}

/* { dg-final { scan-assembler-times {vqwbdotau\.vv} 4 } } */
/* { dg-final { scan-assembler-times {vqwbdotas\.vv} 4 } } */
/* { dg-final { scan-assembler-times {vfqwbdota\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfqwbdota\.alt\.vv} 2 } } */
/* { dg-final { scan-assembler-times {vfwbdota\.vv} 1 } } */
/* { dg-final { scan-assembler-times {vfbdota\.vv} 2 } } */

/* { dg-final { scan-assembler-times {vqwbdotau\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*8} 2 } } */
/* { dg-final { scan-assembler-times {vqwbdotas\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*8} 2 } } */
/* { dg-final { scan-assembler-times {vqwbdotau\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*16} 2 } } */
/* { dg-final { scan-assembler-times {vqwbdotas\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*16} 2 } } */
/* { dg-final { scan-assembler-times {vfqwbdota\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*24} 2 } } */
/* { dg-final { scan-assembler-times {vfqwbdota\.alt\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*32} 2 } } */
/* { dg-final { scan-assembler-times {vfwbdota\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*40} 1 } } */
/* { dg-final { scan-assembler-times {vfbdota\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*48} 1 } } */
/* { dg-final { scan-assembler-times {vfbdota\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+,\s*56} 1 } } */

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e8,\s*m1,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e8alt,\s*m1,\s*ta,\s*ma} 4 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e16,\s*m1,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e16alt,\s*m1,\s*ta,\s*ma} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-z0-9]+,\s*e32,\s*m1,\s*ta,\s*ma} 2 } } */
/* { dg-final { scan-assembler-times {fsrmi\s+[0-4]} 1 } } */
/* { dg-final { scan-assembler-times {fsrm\s+[a-z0-9]+} 1 } } */

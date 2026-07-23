/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvabd -mabi=lp64d" } */
#include <riscv_vector.h>

vuint8m1_t
test_vabs_v_u8m1 (vint8m1_t vs2, size_t vl)
{
  return __riscv_vabs_v_u8m1 (vs2, vl);
}

vuint8m1_t
test_vabs_v_u8m1_tumu (vbool8_t mask, vuint8m1_t vd, vint8m1_t vs2, size_t vl)
{
  return __riscv_vabs_v_u8m1_tumu (mask, vd, vs2, vl);
}

vuint16m1_t
test_vabs_v_u16m1_tu (vuint16m1_t vd, vint16m1_t vs2, size_t vl)
{
  return __riscv_vabs_v_u16m1_tu (vd, vs2, vl);
}

vuint32m1_t
test_vabs_v_u32m1_mu (vbool32_t mask, vuint32m1_t vd, vint32m1_t vs2, size_t vl)
{
  return __riscv_vabs_v_u32m1_mu (mask, vd, vs2, vl);
}

vuint64m1_t
test_vabs_v_u64m1_m (vbool64_t mask, vint64m1_t vs2, size_t vl)
{
  return __riscv_vabs_v_u64m1_m (mask, vs2, vl);
}

vuint16m1_t
test_vabd_vv_u16m1 (vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_vabd_vv_u16m1 (vs2, vs1, vl);
}

vuint8m1_t
test_vabd_vv_u8m1_m (vbool8_t mask, vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_vabd_vv_u8m1_m (mask, vs2, vs1, vl);
}

vuint16m1_t
test_vabdu_vv_u16m1 (vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_vabdu_vv_u16m1 (vs2, vs1, vl);
}

vuint16m1_t
test_vabdu_vv_u16m1_tu (vuint16m1_t vd, vuint16m1_t vs2, vuint16m1_t vs1,
			size_t vl)
{
  return __riscv_vabdu_vv_u16m1_tu (vd, vs2, vs1, vl);
}

vuint32m1_t
test_vwabda_vv_u32m1 (vuint32m1_t vd, vint16mf2_t vs2, vint16mf2_t vs1,
		      size_t vl)
{
  return __riscv_vwabda_vv_u32m1 (vd, vs2, vs1, vl);
}

vuint16m1_t
test_vwabda_vv_u16m1_tu (vuint16m1_t vd, vint8mf2_t vs2, vint8mf2_t vs1,
			 size_t vl)
{
  return __riscv_vwabda_vv_u16m1_tu (vd, vs2, vs1, vl);
}

vuint16m1_t
test_vwabdau_vv_u16m1 (vuint16m1_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1,
		       size_t vl)
{
  return __riscv_vwabdau_vv_u16m1 (vd, vs2, vs1, vl);
}

vuint16m1_t
test_vwabdau_vv_u16m1_mu (vbool16_t mask, vuint16m1_t vd, vuint8mf2_t vs2,
			  vuint8mf2_t vs1, size_t vl)
{
  return __riscv_vwabdau_vv_u16m1_mu (mask, vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times {\tvabs\.v} 5 } } */
/* { dg-final { scan-assembler-times {\tvabd\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvabdu\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvwabda\.vv} 2 } } */
/* { dg-final { scan-assembler-times {\tvwabdau\.vv} 2 } } */

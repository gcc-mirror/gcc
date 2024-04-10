/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vint8mf8_t test_vaadd_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vaadd_vv_i8mf8 (op1, op2, __RISCV_VXRM_RNU, vl);
}

vuint8mf8_t test_vaaddu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vaaddu_vv_u8mf8 (op1, op2, __RISCV_VXRM_RNU, vl);
}

vint8mf8_t test_vasub_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vasub_vv_i8mf8 (op1, op2, __RISCV_VXRM_RNU, vl);
}

vuint8mf8_t test_vasubu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vasubu_vv_u8mf8 (op1, op2, __RISCV_VXRM_RNU, vl);
}

vint8mf8_t test_vnclip_wv_i8mf8 (vint16mf4_t op1, vuint8mf8_t shift, size_t vl)
{
  return __riscv_vnclip_wv_i8mf8 (op1, shift, __RISCV_VXRM_RNU, vl);
}

vuint8mf8_t test_vnclipu_wv_u8mf8 (vuint16mf4_t op1, vuint8mf8_t shift, size_t vl)
{
  return __riscv_vnclipu_wv_u8mf8 (op1, shift, __RISCV_VXRM_RNU, vl);
}

vint8mf8_t test_vsmul_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vsmul_vv_i8mf8 (op1, op2, __RISCV_VXRM_RNU, vl);
}

vint8mf8_t test_vssra_vv_i8mf8 (vint8mf8_t op1, vuint8mf8_t shift, size_t vl)
{
  return __riscv_vssra_vv_i8mf8 (op1, shift, __RISCV_VXRM_RNU, vl);
}

vuint8mf8_t test_vssrl_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t shift, size_t vl)
{
  return __riscv_vssrl_vv_u8mf8 (op1, shift, __RISCV_VXRM_RNU, vl);
}

vint8mf8_t test_vsadd_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vsadd_vv_i8mf8 (op1, op2, vl);
}
vuint8mf8_t test_vsaddu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vsaddu_vv_u8mf8 (op1, op2, vl);
}
vint8mf8_t test_vssub_vv_i8mf8 (vint8mf8_t op1, vint8mf8_t op2, size_t vl)
{
  return __riscv_vssub_vv_i8mf8 (op1, op2, vl);
}
vuint8mf8_t test_vssubu_vv_u8mf8 (vuint8mf8_t op1, vuint8mf8_t op2, size_t vl)
{
  return __riscv_vssubu_vv_u8mf8 (op1, op2, vl);
}

/* { dg-final { scan-assembler-times {vaadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vaaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vasub\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vasubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vnclip\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vnclipu\.wv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsmul\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssra\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssrl\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsadd\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vsaddu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssub\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {vssubu\.vv\s+v[0-9]+,\s*v[0-9]+,\s*v[0-9]+} 1 } } */
/* { dg-final { scan-assembler-times {csrwi\s+vxrm,\s*0} 9 } } */

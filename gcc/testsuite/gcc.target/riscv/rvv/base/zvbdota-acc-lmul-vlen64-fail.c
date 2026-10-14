/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zvqwbdota16i_zvl64b -mabi=lp64d -O2" } */

#include "riscv_vector.h"

vuint64m1_t
test_vqwbdota_u64m1 (vuint64m1_t vd, vuint16m8_t vs2, vuint16m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m1 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl512b ISA extension" "" { target *-*-* } .-1 } */
}

vuint64m2_t
test_vqwbdota_u64m2 (vuint64m2_t vd, vuint16m8_t vs2, vuint16m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m2 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl256b ISA extension" "" { target *-*-* } .-1 } */
}

vuint64m4_t
test_vqwbdota_u64m4 (vuint64m4_t vd, vuint16m8_t vs2, vuint16m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m4 (vd, vs2, vs1, 0, vl);
  /* { dg-error "requires the zvl128b ISA extension" "" { target *-*-* } .-1 } */
}

vuint64m8_t
test_vqwbdota_u64m8 (vuint64m8_t vd, vuint16m8_t vs2, vuint16m1_t vs1,
		     size_t vl)
{
  return __riscv_vqwbdota_vv_u16m8_u16m1_u64m8 (vd, vs2, vs1, 0, vl);
}

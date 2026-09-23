/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvkg_zvkned_zvksed -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv_zvkg_zvkned_zvksed -mabi=ilp32d -O2" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

#include <riscv_vector.h>

vuint32m1_t
f (vuint32m1_t a, size_t vl)
{
  vuint32m1_t r = __riscv_vaesdf_vs_u32m1_u32m1 (a, a, vl);
  r = __riscv_vaesdm_vs_u32m1_u32m1 (r, r, vl);
  r = __riscv_vaesef_vs_u32m1_u32m1 (r, r, vl);
  r = __riscv_vaesem_vs_u32m1_u32m1 (r, r, vl);
  r = __riscv_vaesz_vs_u32m1_u32m1 (r, r, vl);
  return __riscv_vsm4r_vs_u32m1_u32m1 (r, r, vl);
}

vuint32m1_t
g (vuint32m1_t a, size_t vl)
{
  vuint32m1_t r = __riscv_vaesdf_vv_u32m1 (a, a, vl);
  r = __riscv_vaesdm_vv_u32m1 (r, r, vl);
  r = __riscv_vaesef_vv_u32m1 (r, r, vl);
  r = __riscv_vaesem_vv_u32m1 (r, r, vl);
  r = __riscv_vsm4r_vv_u32m1 (r, r, vl);
  return __riscv_vgmul_vv_u32m1 (r, r, vl);
}

/* { dg-final { scan-assembler-not {vaesdf\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler-not {vaesdm\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler-not {vaesef\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler-not {vaesem\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler-not {vaesz\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler-not {vsm4r\.vs\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vaesdf\.vv\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vaesdm\.vv\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vaesef\.vv\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vaesem\.vv\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vsm4r\.vv\tv([0-9]+),v\1\s} } } */
/* { dg-final { scan-assembler {vgmul\.vv\tv([0-9]+),v\1\s} } } */

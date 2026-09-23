/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvknhb_zvkg -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv_zvknhb_zvkg -mabi=ilp32d -O2" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

#include <riscv_vector.h>

vuint32m1_t
f (vuint32m1_t a, vuint32m1_t b, size_t vl)
{
  vuint32m1_t r = __riscv_vsha2ms_vv_u32m1 (a, a, b, vl);
  r = __riscv_vsha2ms_vv_u32m1 (r, b, r, vl);
  r = __riscv_vsha2ch_vv_u32m1 (r, r, b, vl);
  r = __riscv_vsha2ch_vv_u32m1 (r, b, r, vl);
  r = __riscv_vsha2cl_vv_u32m1 (r, r, b, vl);
  r = __riscv_vsha2cl_vv_u32m1 (r, b, r, vl);
  return __riscv_vghsh_vv_u32m1 (r, r, b, vl);
}

/* { dg-final { scan-assembler-not {vsha2ms\.vv\tv([0-9]+),v\1,} } } */
/* { dg-final { scan-assembler-not {vsha2ms\.vv\tv([0-9]+),v[0-9]+,v\1\s} } } */
/* { dg-final { scan-assembler-not {vsha2ch\.vv\tv([0-9]+),v\1,} } } */
/* { dg-final { scan-assembler-not {vsha2ch\.vv\tv([0-9]+),v[0-9]+,v\1\s} } } */
/* { dg-final { scan-assembler-not {vsha2cl\.vv\tv([0-9]+),v\1,} } } */
/* { dg-final { scan-assembler-not {vsha2cl\.vv\tv([0-9]+),v[0-9]+,v\1\s} } } */
/* { dg-final { scan-assembler {vghsh\.vv\tv([0-9]+),v\1,} } } */

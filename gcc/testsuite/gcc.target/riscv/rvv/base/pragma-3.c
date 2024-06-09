/* { dg-do compile } */
/* { dg-skip-if "test rvv intrinsic" { ! riscv_v } } */

#pragma riscv intrinsic "report-error" /* { dg-error {unknown '#pragma riscv intrinsic' option 'report-error'} } */

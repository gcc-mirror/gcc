/* { dg-do compile } */
/* { dg-skip-if "test rvv intrinsic" { *-*-* } { "*" } { "-march=rv*v*" } } */

#pragma riscv intrinsic "report-error" /* { dg-error {unknown '#pragma riscv intrinsic' option 'report-error'} } */

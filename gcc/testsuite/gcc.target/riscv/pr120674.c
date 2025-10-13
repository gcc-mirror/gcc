/* { dg-do compile } */
/* { dg-additional-options "-g -w -march=rv32gcv -mabi=ilp32" { target rv32 } } */
/* { dg-additional-options "-g -w -march=rv64gcv -mabi=lp64d" { target rv64 } } */

#pragma riscv intrinsic "vector"
void GB_AxB_saxpy5_unrolled_rvv() { vfloat64m8_t vc; }

/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xandesbfhcvt -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xandesbfhcvt -mabi=lp64" { target { rv64 } } } */

__bf16
nds_fcvt_bf16_s (float a)
{
  return __builtin_riscv_nds_fcvt_bf16_s (a);
}

/* { dg-final { scan-assembler-times {nds\.fcvt\.bf16\.s} 1 } } */

/* { dg-do compile } */
/* { dg-options "-march=rv32gc_xandesbfhcvt -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xandesbfhcvt -mabi=lp64" { target { rv64 } } } */

float
nds_fcvt_s_bf16 (__bf16 a)
{
  return __builtin_riscv_nds_fcvt_s_bf16 (a);
}

/* { dg-final { scan-assembler-times {nds\.fcvt\.s\.bf16} 1 } } */

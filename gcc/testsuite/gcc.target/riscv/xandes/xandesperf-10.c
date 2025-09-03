/* { dg-do compile } */
/* { dg-require-effective-target rv32 } */
/* { dg-options "-march=rv32gc_xandesperf -mabi=ilp32" } */

int
test_nds_ffb (unsigned int a, unsigned int b)
{
  return __builtin_riscv_nds_ffb_32 (a, b);
}

int
test_nds_ffzmism (unsigned int a, unsigned int b)
{
  return __builtin_riscv_nds_ffzmism_32 (a, b);
}

int
test_nds_ffmism (unsigned int a, unsigned int b)
{
  return __builtin_riscv_nds_ffmism_32 (a, b);
}

int
test_nds_flmism (unsigned int a, unsigned int b)
{
  return __builtin_riscv_nds_flmism_32 (a, b);
}

/* { dg-final { scan-assembler-times {nds\.ffb} 1 } } */
/* { dg-final { scan-assembler-times {nds\.ffzmism} 1 } } */
/* { dg-final { scan-assembler-times {nds\.ffmism} 1 } } */
/* { dg-final { scan-assembler-times {nds\.flmism} 1 } } */

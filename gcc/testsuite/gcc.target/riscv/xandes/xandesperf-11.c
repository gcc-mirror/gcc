/* { dg-do compile } */
/* { dg-require-effective-target rv64 } */
/* { dg-options "-march=rv64gc_xandesperf -mabi=lp64" } */

long
test_nds_ffb (unsigned long a, unsigned long b)
{
  return __builtin_riscv_nds_ffb_64 (a, b);
}

long
test_nds_ffzmism (unsigned long a, unsigned long b)
{
  return __builtin_riscv_nds_ffzmism_64 (a, b);
}

long
test_nds_ffmism (unsigned long a, unsigned long b)
{
  return __builtin_riscv_nds_ffmism_64 (a, b);
}

long
test_nds_flmism (unsigned long a, unsigned long b)
{
  return __builtin_riscv_nds_flmism_64 (a, b);
}

/* { dg-final { scan-assembler-times {nds\.ffb} 1 } } */
/* { dg-final { scan-assembler-times {nds\.ffzmism} 1 } } */
/* { dg-final { scan-assembler-times {nds\.ffmism} 1 } } */
/* { dg-final { scan-assembler-times {nds\.flmism} 1 } } */

/* { dg-do compile { target { ! riscv_abi_e } } } */
/* { dg-options "-march=rv32gc_xtheadmac" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadmac" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" } } */

long f_mula(long a, long b, long c)
{
  return a + b * c;
}

long f_muls(long a, long b, long c)
{
  return a - b * c;
}

#if __riscv_xlen == 64
int f_mulaw(int a, int b, int c)
{
  return a + b * c;
}

int f_mulsw(int a, int b, int c)
{
  return a - b * c;
}
#endif

long f_mulah(int a, unsigned short b, unsigned short c)
{
  return a + (int)(short)b * (int)(short)c;
}

long f_mulsh(int a, unsigned short b, unsigned short c)
{
  return a - (int)(short)b * (int)(short)c;
}

/* { dg-final { scan-assembler-times "th.mula\t" 1 } } */
/* { dg-final { scan-assembler-times "th.muls\t" 1 } } */
/* { dg-final { scan-assembler-times "th.mulaw\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.mulsw\t" 1 { target { rv64 } } } } */
/* { dg-final { scan-assembler-times "th.mulah\t" 1 } } */
/* { dg-final { scan-assembler-times "th.mulsh\t" 1 } } */

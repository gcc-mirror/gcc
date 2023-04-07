/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64 -O2" } */

unsigned long long f5(unsigned long long i)
{
  return i * 0x0202020202020202ULL;
}

/* { dg-final { scan-assembler-times "mul" 1 } } */

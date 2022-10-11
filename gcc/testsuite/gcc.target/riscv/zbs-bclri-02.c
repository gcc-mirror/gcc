/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

long long f3(long long a)
{
  return a & ~0x1100;
}

long long f4 (long long a)
{
  return a & ~0x80000000000000ffull;
}

long long f5 (long long a)
{
  return a & ~0x8000001000000000ull;
}

long long f6 (long long a)
{
  return a & ~0xff7ffffffffffffull;
}

/* { dg-final { scan-assembler-times "bclri\t" 4 } } */
/* { dg-final { scan-assembler-times "andi\t" 2 } } */


/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" "-Os" "-Oz" } } */

long long foo0(long long a, long long b)
{
  if (a >= 0)
    return b;

  return 0;
}

/* { dg-final { scan-assembler-times "srai\t" 1 } } */
/* { dg-final { scan-assembler-times "andn\t" 1 } } */


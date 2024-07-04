/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* bext */
long
foo0 (long i, long j)
{
  return 1L & (i >> j);
}

/* bexti */
long
foo1 (long i)
{
  return 1L & (i >> 20);
}

long bext64_1(long a, char bitno)
{
  return (a & (1UL << bitno)) ? 1 : 0;
}

long bext64_2(long a, char bitno)
{
  return (a & (1UL << bitno)) ? 0 : 1;
}

long bext64_3(long a, char bitno)
{
  return (a & (1UL << bitno)) ? 0 : -1;
}

long bext64_4(long a, char bitno)
{
  return (a & (1UL << bitno)) ? -1 : 0;
}

/* { dg-final { scan-assembler-times "bexti\t" 1 } } */
/* { dg-final { scan-assembler-times "bext\t" 5 } } */
/* { dg-final { scan-assembler-times "xori\t|seqz\t" 1 } } */
/* { dg-final { scan-assembler-times "addi\t" 1 } } */
/* { dg-final { scan-assembler-times "neg\t" 1 } } */
/* { dg-final { scan-assembler-not {\mandi} } } */

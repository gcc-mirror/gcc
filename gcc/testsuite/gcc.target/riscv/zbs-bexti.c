/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* bexti */
#define BIT_NO  21

long bexti64_1(long a, char bitno)
{
  return (a & (1UL << BIT_NO)) ? 1 : 0;
}

long bexti64_2(long a, char bitno)
{
  return (a & (1UL << BIT_NO)) ? 0 : 1;
}

long bexti64_3(long a, char bitno)
{
  return (a & (1UL << BIT_NO)) ? 0 : -1;
}

long bexti64_4(long a, char bitno)
{
  return (a & (1UL << BIT_NO)) ? -1 : 0;
}

/* { dg-final { scan-assembler-times "bexti\t" 4 } } */
/* { dg-final { scan-assembler-times "xori\t" 1 } } */
/* { dg-final { scan-assembler-times "addi\t" 1 } } */
/* { dg-final { scan-assembler-times "neg\t" 1 } } */

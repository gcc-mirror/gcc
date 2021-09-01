/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64 -O2" } */

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

/* { dg-final { scan-assembler-times "bexti\t" 1 } } */
/* { dg-final { scan-assembler-times "bext\t" 1 } } */
/* { dg-final { scan-assembler-not "andi" } } */

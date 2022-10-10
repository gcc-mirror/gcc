/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

/* bexti */
int f(int* a, int b)
{
  return ((*a << b) | (1 << 14));
}

int g(int a, int b)
{
  return ((a + b)| (1 << 30));
}

int h(int a, int b)
{
  return ((a + b)| (1ULL << 33));
}

/* { dg-final { scan-assembler-times "addw\t" 2 } } */
/* { dg-final { scan-assembler-times "sllw\t" 1 } } */
/* { dg-final { scan-assembler-times "bseti\t" 2 } } */
/* { dg-final { scan-assembler-not "sext.w\t" } } */


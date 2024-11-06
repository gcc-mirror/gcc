/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl1024b -mabi=lp64d -O3 -fdump-tree-optimized-details" } */

struct S { int a, b; } s[8];

void
foo ()
{
  int i;
  for (i = 0; i < 8; i++)
    {
      s[i].b = 1;
      s[i].a = i+1;
    }
}

/* { dg-final { scan-tree-dump-times "\{ 1, 1, 2, 1, 3, 1, 4, 1 \}" 1 "optimized" } } */
/* { dg-final { scan-assembler {vid\.v} } } */
/* { dg-final { scan-assembler {vadd\.v} } } */
/* { dg-final { scan-assembler {vor\.v} } } */

void
foo2 ()
{
  int i;
  for (i = 0; i < 8; i++)
    {
      s[i].b = 0;
      s[i].a = i+1;
    }
}

/* { dg-final { scan-tree-dump-times "\{ 1, 0, 2, 0, 3, 0, 4, 0 \}" 1 "optimized" } } */
/* { dg-final { scan-assembler {vid\.v} } } */
/* { dg-final { scan-assembler {vadd\.v} } } */

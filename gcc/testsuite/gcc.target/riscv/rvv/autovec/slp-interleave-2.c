/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvl1024b -mabi=lp64d -fno-vect-cost-model -mrvv-max-lmul=m8 -O3 -fdump-tree-optimized-details" } */

struct S { int a, b; } s[8];

void
foo ()
{
  int i;
  for (i = 0; i < 8; i++)
    {
      s[i].b = 1;
      s[i].a = i;
    }
}

/* { dg-final { scan-tree-dump-times "\{ 0, 1, 1, 1, 2, 1, ... \}" 1 "optimized" } } */
/* { dg-final { scan-assembler-times {slli\t[a-x0-9]+,\s*[a-x0-9]+,\s*32} 1 } } */

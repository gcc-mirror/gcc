/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvl1024b -mabi=lp64d -fno-vect-cost-model -mrvv-max-lmul=m8 -O3 -fdump-tree-optimized-details" } */

struct S { int a, b; } s[8];

void
foo ()
{
  int i;
  for (i = 0; i < 8; i++)
    {
      s[i].b = i*3 + 100;
      s[i].a = i + 200;
    }
}

/* { dg-final { scan-tree-dump-times "\{ 200, 100, 201, 103, 202, 106, ... \}" 1 "optimized" } } */
/* { dg-final { scan-assembler-times {vand\.vi} 1 } } */
/* { dg-final { scan-assembler-times {vmseq\.vi} 1 } } */

/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-optimized-details-blocks" } */
/* { dg-additional-options "-mavx512bw -mavx512vl" { target { i?86-*-* x86_64-*-* } } } */

char a[32];
char b[32];
char c[32];

void test()
{
  int i = 0;
  for (i = 0; i < 32; i++)
    if (b[i] > 0)
      a[i] = c[i];
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" { target { i?86-*-* x86_64-*-* } } } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */

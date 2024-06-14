/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse4 -fno-tree-slp-vectorize" } */

void foo (int * __restrict a, int *b, int s)
{
  if (s >= 6)
    for (int i = 0; i < 1024; ++i)
      {
	a[s*i+0] = b[4*i+0];
	a[s*i+1] = b[4*i+1];
	a[s*i+2] = b[4*i+2];
	a[s*i+3] = b[4*i+3];
	a[s*i+4] = b[4*i+0];
	a[s*i+5] = b[4*i+1];
      }
}

/* While the vectorizer generates 6 uint64 stores.  */
/* { dg-final { scan-assembler-times "movq" 4 } } */
/* { dg-final { scan-assembler-times "movhps" 2 } } */

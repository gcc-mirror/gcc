/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-vectorize -fdump-tree-vect-details -fno-tree-loop-im -msse2 -mno-avx" } */

double x[1024][1024], red[1024];
void foo (void)
{
  for (int i = 0; i < 1024; ++i)
    for (int j = 0; j < 1024; ++j)
      {
	double v = i == 0 ? 0.0 : red[j];
	v = v + x[i][j];
	red[j] = v;
      }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */

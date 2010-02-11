/* { dg-options "-O3 -fgraphite-identity" } */

extern int *A;
extern int B[][4];
extern void bar(void);

void foo(int im, int jm, int cond)
{
  int i, j;
  if (cond) {
    for (i = 0; i < 256; i++)
      A[i] = 0;
    bar();
  }
  for (i = 0; i < im; i++)
    for (j = 1; j < jm; j++)
      if (jm != 8 || j != jm >> 1)
        B[j][0] ^= B[j-1][0];
}

/* { dg-options "-O1 -fstrict-overflow -fgraphite-identity" } */

void foo(int x[])
{
  int i, j;
  for (i = 0; i < 2; i++)
    for (j = 0; j < 2; j++)
      x[i] = x[i*j];
}

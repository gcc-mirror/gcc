/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution" } */

int a[8][8];
int b[8];

void
foo (int i)
{
  int j;
  for (j = 0; j < 8; j++)
    {
      a[i][j] = 0;
      b[j] = j;
    }
}

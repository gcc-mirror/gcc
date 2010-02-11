/* { dg-options "-O2 -g -ffast-math -floop-parallelize-all" } */

int array[2][2];

void foo(int *a)
{
  int i, j;
  int sum, tmp = 0;

  for (i=0; i<2; i++)
    for (j=0; j<2; j++)
      sum += array[i][j];

  if (sum > 0) {
    tmp = sum;
    *a = tmp;
  }
}

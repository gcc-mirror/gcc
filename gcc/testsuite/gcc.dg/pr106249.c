/* { dg-do compile } */
/* { dg-options "-O -floop-unroll-and-jam --param unroll-jam-min-percent=0" } */

void
foo (double *arr)
{
  int i, j;

  for (i = 0; i < 4; ++i)
    for (j = 0; j < 4; ++j)
      arr[j] = 0;

  for (i = 1; i < 4; ++i)
    for (j = 0; j < 4; ++j)
      arr[j] = 1.0 / (i + 1);
}

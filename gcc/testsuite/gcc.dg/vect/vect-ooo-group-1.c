/* { dg-do compile } */

void
f (int *restrict a, int *restrict b, int *restrict c)
{
  for (int i = 0; i < 100; ++i)
    if (c[i])
      {
	a[i * 2] = b[i * 5 + 2];
	a[i * 2 + 1] = b[i * 5];
      }
}

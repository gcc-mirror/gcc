/* { dg-do compile } */
/* { dg-options "-O -ftree-loop-linear" } */

int
foo (int a[4096], int mi, int mj)
{
  int i, j, i16;
  for (i = 0; i < mi; i++)
    {
      i16 = i + 16;
      for (j = 0; j < mj; j++)
	i16 = a[j] == i16;
    }
  return i16;
}

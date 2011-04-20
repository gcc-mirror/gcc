/* PR 47892 */
/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_condition } */

void
bestseries9 (float * __restrict__ arr, int len)
{
  int i;
  for (i = 0; i < len; ++i)
    {
      float or = arr[i];
      arr[i] = (or > 0.0f) * (2 - or * or);
    }
}

/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
/* { dg-final { cleanup-tree-dump "vect" } } */

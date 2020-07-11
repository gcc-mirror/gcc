/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -fdump-tree-vect" } */

void
foo (double *a, double *b, double m, int inc_x, int inc_y)
{
  int ix = 0, iy = 0;
  for (int i = 0; i < 1000; ++i)
    {
      a[ix] += m * b[iy];
      ix += inc_x;
      iy += inc_y;
    }
  return ;
}

/* { dg-final { scan-tree-dump-times "VEC_SERIES_EXPR" 2 "vect" } } */

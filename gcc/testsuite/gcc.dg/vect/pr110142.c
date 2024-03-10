/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
void test(short *x, unsigned short *y, int n)
{
  for (int i = 0; i < n; i++)
      x[i] = (y[i] - x[i]) >> 1;
}

/* { dg-final { scan-tree-dump-not "widen_minus" "vect" } } */

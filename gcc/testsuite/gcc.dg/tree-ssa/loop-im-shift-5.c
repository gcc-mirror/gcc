/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-lim2-details" } */

/* When optimizing for size a shift that would need a mask stays put.  */

void g (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << s) * 5) / 7;
}

/* { dg-final { scan-tree-dump-not "Moving statement" "lim2" } } */

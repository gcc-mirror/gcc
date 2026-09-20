/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details -fdump-tree-optimized" } */

/* The count is already in range: hoisted without a further mask.  */

void f (int *p, int n, int a, unsigned int s, int c, int *q)
{
  s &= 7;
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << s) * 5) / 7;
}

/* An unconstrained count is masked at the hoisted shift.  */

void g (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << s) * 5) / 7;
}

/* A mask that does not bound the count below the precision is refined.  */

void h (int *p, int n, int a, unsigned int s, int c, int *q)
{
  s &= 63;
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = ((a << s) * 5) / 7;
}

/* The guard's range is exported as global by VRP, so no mask survives.  */

void k (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      {
	if (s > 7) __builtin_unreachable ();
	q[i] = ((a << s) * 5) / 7;
      }
}

/* A lone shift that would need a mask is not worth moving by itself.  */

void m (int *p, int n, int a, unsigned int s, int c, int *q)
{
  for (int i = 0; i < n; i++)
    if (p[i] > c)
      q[i] = a << s;
}

/* Every chain but m's moves: the shift, the multiply and the divide.  */
/* { dg-final { scan-tree-dump-times "Moving statement" 12 "lim2" } } */
/* { dg-final { scan-tree-dump-times "Moving statement \[^\\r\\n\]*<<" 4 "lim2" } } */
/* f's count was masked by 7 already; g and h get a mask of 31.  */
/* { dg-final { scan-tree-dump-times " & 31;" 2 "optimized" } } */

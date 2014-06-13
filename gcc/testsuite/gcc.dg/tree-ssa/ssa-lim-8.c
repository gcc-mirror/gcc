/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-lim1-details" } */

void bar (int);
void foo (int n, int m)
{
  unsigned i;
  for (i = 0; i < n; ++i)
    {
      int x;
      if (m < 0)
	x = 1;
      else
	x = m;
      bar (x);
    }
}

/* { dg-final { scan-tree-dump-times "Moving PHI node" 1 "lim1"  } } */
/* { dg-final { cleanup-tree-dump "lim1" } } */

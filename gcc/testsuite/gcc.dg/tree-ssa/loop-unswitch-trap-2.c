/* { dg-do compile } */
/* { dg-options "-O3 -ftrapping-math -fdump-tree-unswitch-details" } */

volatile int sink;
int a[16];

void
f (double x, int n)
{
  for (int i = 0; i < n; i++)
    if (a[i])
      {
	if (x < 1.0)
	  sink = 1;
	else
	  sink = 2;
      }
}

/* { dg-final { scan-tree-dump-not "unswitching loop" "unswitch" } } */

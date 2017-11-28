/* { dg-do compile } */
/* { dg-options "-O3 -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

#define k 1335
double a[k][k];
double b[k][k];
double c[k][k];

int x;
int y;

void
foo (void)
{
  for (int j = 0; j < x; j++)
    {
      for (int i = 0; i < y; i++)
	{
	  c[j][i] = b[j][i] - a[j][i];
	  a[j][i] = b[j][i];
	}
    }
}

/* { dg-final { scan-tree-dump "Loop . not distributed" "ldist" } } */

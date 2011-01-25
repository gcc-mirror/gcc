/* PR tree-optimization/29581 */
/* Origin: gcc.dg/vect/vect-87.c */
/* { dg-do run } */
/* { dg-options "-O2 -ftree-loop-linear" } */

extern void abort (void);

#define N 16

int main1 (int n, int *a)
{
  int i, j, k;
  int b[N];

  for (i = 0; i < n; i++)
    {
      for (j = 0; j < n; j++)
	{
	  k = i + n;
	  a[j] = k;
	}
      b[i] = k;
    }


  for (j = 0; j < n; j++)
    if (a[j] != i + n - 1)
      abort();	

  for (j = 0; j < n; j++)
    if (b[j] != j + n)
      abort();	

  return 0;
}

int main (void)
{
  int a[N] __attribute__ ((__aligned__(16)));

  main1 (N, a);
  main1 (0, a);
  main1 (1, a);
  main1 (2, a);
  main1 (N-1, a);

  return 0;
}

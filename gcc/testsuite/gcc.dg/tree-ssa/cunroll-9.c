/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cunrolli-details -fdisable-tree-evrp" } */
void abort (void);
int q (void);
int a[10];
int b[11];
int
t (int n)
{
  int i;
  int sum = 0;
  for (i = 0; i < n; i++)
    {
      if (i > 1000)
	abort ();
      if (q ())
	sum += a[i];
      else
	sum += b[i];
    }
  return sum;
}
/* { dg-final { scan-tree-dump-times "Removed pointless exit:" 1 "cunrolli" } } */

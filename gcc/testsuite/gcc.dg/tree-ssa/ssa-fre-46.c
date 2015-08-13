/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */

int x[1024];
int foo (int a, int s, unsigned int k)
{
  int i = a, j = a;
  int sum = 0;
  do
    {
      sum += x[i];
      sum += x[j];
      i += s;
      j += s;
    }
  while (k--);
  return sum;
}

/* We want to remove the redundant induction variable and thus its PHI node.  */
/* { dg-final { scan-tree-dump "Removing dead stmt \[^\r\n\]*PHI" "fre1" } } */

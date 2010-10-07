/* { dg-do compile } */


long long foo (long long *__restrict a,  int *__restrict b, int *__restrict c )
{
  int i;
  long long sum=0;
  for (i=0;i<256;i++)
   sum += (long long)b[i] * c[i];

  return sum;
}

/* { dg-final { cleanup-tree-dump "vect" } } */


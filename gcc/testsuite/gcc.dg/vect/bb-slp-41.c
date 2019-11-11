/* { dg-do run } */
/* { dg-options "-O3 -fdump-tree-slp-all -fno-vect-cost-model" } */
/* { dg-require-effective-target vect_int } */

#define ARR_SIZE 1000

void foo (int *a, int *b)
{
  int i;
  for (i = 0; i < (ARR_SIZE - 2); ++i)
    a[i] = b[0] + b[1] + b[i+1] + b[i+2];
}

void bar (int *a, int *b)
{
  int i;
  for (i = 0; i < (ARR_SIZE - 2); ++i)
  {
    a[i] = b[0];
  }
  for (i = 0; i < (ARR_SIZE - 2); ++i)
  {
    a[i] = a[i] + b[1];
  }
  for (i = 0; i < (ARR_SIZE - 2); ++i)
  {
    a[i] = a[i] + b[i+1];
  }
  for (i = 0; i < (ARR_SIZE - 2); ++i)
  {
    a[i] = a[i] + b[i+2];
  }
}

int main ()
{
  int a1[ARR_SIZE];
  int a2[ARR_SIZE];
  int b[ARR_SIZE];
  int i;

  for (i = 0; i < ARR_SIZE; i++)
  {
    a1[i] = 0;
    a2[i] = 0;
    b[i]  = i;
  }

  foo (a1, b);
  bar (a2, b);

  for (i = 0; i < ARR_SIZE; i++)
    if (a1[i] != a2[i])
      return 1;

  return 0;

}
/* See that we vectorize an SLP instance.  */
/* { dg-final { scan-tree-dump-times "Found vectorizable constructor" 12 "slp1" } } */
/* { dg-final { scan-tree-dump-times "vectorizing stmts using SLP" 4 "slp1" } } */

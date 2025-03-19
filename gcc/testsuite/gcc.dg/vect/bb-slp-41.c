/* { dg-require-effective-target vect_int } */

#define ARR_SIZE 1000

void __attribute__((optimize (0)))
foo (int *a, int *b)
{
  int i;
  for (i = 0; i < (ARR_SIZE - 2); ++i)
    a[i] = b[0] + b[1] + b[i+1] + b[i+2];
}

/* Disable pre-slp FRE to avoid unexpected SLP on the epilogue
   of the 1st loop.  */
void __attribute__((optimize("-fno-tree-fre")))
bar (int *a, int *b)
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

#pragma GCC novector
  for (i = 0; i < ARR_SIZE; i++)
    if (a1[i] != a2[i])
      return 1;

  return 0;

}
/* { dg-final { scan-tree-dump-not "vectorizing stmts using SLP" "slp1" } } */

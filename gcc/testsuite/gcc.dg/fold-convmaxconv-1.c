/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int foo (short a[], int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (x <= 0 ? 0 : x);
    }
  return x;
}

/* { dg-final { scan-tree-dump-not " = MAX_EXPR <x_\[0-9\]*" "optimized" } } */

/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ldist-details" } */

int x[1000];

void foo (int n)
{
  int i;

  for (i = 0; i < n; ++i)
    {
      x[2*i] = 0;
      x[2*i + 1] = 1;
    }
}

/* We should not apply loop distribution as it is not beneficial from
   a data locality point of view.  Also it is not possible to generate
   a memset (0) as the write has a stride of 2.  */

/* { dg-final { scan-tree-dump-not "distributed: split to" "ldist" } } */
/* { dg-final { scan-tree-dump-not "__builtin_memset" "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */

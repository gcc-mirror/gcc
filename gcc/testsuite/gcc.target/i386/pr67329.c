/* { dg-do compile { target ia32 } } */
/* { dg-options "-O3 -fno-tree-fre -fno-tree-pre -fdump-tree-optimized -mtune=lakemont" } */

int
foo ()
{
  const int a[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int i, sum;

  sum = 0;
  for (i = 0; i < sizeof (a) / sizeof (*a); i++)
    sum += a[i];

  return sum;
}

/* After late unrolling the above loop completely DOM should be
   able to optimize this to return 28.  */

/* { dg-final { scan-tree-dump "return 28;" "optimized" } } */

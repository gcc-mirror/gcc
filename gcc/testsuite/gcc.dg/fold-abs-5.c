/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

int test (int a, int b, int sum)
{
  sum += ((a - b) > 0 ? (a - b) : -(a - b));
  return sum;
}

/* { dg-final { scan-tree-dump "ABS" "original" } } */

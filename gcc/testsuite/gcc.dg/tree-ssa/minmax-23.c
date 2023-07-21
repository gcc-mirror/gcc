/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-reassoc -fdump-tree-optimized" } */


#define MAX(a,b) (a)>=(b) ? (a) : (b)

#define MIN(a,b) (a)<=(b) ? (a) : (b)

int test1(int a, int b)
{
  int d = MAX(a,b);
  return MAX(a,d);
}
int test2(int a, int b)
{
  int d = MIN(a,b);
  return MIN(a,d);
}

/* We should be optimize these two functions even without reassociation. */
/* { dg-final { scan-tree-dump-times "MAX_EXPR " 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 1 "optimized"} } */

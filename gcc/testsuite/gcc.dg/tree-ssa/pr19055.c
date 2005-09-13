/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */
int f1(int a,int b)
{
  return (a|b) ^ b;
}
int f2(int a,int b)
{
  return (b|a) ^ b;
}
int f3(int a,int b)
{
  return b^(a|b);
}
int f4(int a,int b)
{
  return b^(b|a);
}
/* There should be no ^, 4 ~ and 4 &. */
/* { dg-final { scan-tree-dump-times "\\^" 0 "gimple"} } */
/* { dg-final { scan-tree-dump-times "~" 4 "gimple"} } */
/* { dg-final { scan-tree-dump-times "&" 4 "gimple"} } */
/* { dg-final { cleanup-tree-dump "gimple" } } */

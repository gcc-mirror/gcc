/* { dg-do compile } */
/* { dg-options "-O1 -fno-semantic-interposition -fdump-tree-optimized -fPIC" } */
int t(void)
{
  return 1;
}
int q(void)
{
  return t();
}
/* { dg-final { scan-tree-dump-times "return 1" 2 "optimized"} } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

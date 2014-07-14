/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1" } */
inline void t()
{
}
int m()
{
  void *q = (void *)&t;
  return q != 0;
}
/* { dg-final { scan-tree-dump "return 1" "ccp1"} } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */

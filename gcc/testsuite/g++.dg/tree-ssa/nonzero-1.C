/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ccp1 -fdelete-null-pointer-checks" } */
/* { dg-skip-if "" keeps_null_pointer_checks } */

inline void t()
{
}
int m()
{
  void *q = (void *)&t;
  return q != 0;
}
/* { dg-final { scan-tree-dump "return 1" "ccp1"} } */

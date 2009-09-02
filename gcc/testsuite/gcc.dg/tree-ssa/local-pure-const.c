/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-local-pure-const1" } */
/* { dg-options "-O1 -fdump-tree-local-pure-const1 -fpie" { target { ! nonpic } } } */
t(int a, int b, int c)
{
  int *p;
  if (a)
    p = &a;
  else
    p = &c;
  return *p;
}
/* { dg-final { scan-tree-dump-times "local memory is OK" 1 "local-pure-const1"} } */
/* { dg-final { scan-tree-dump-times "found to be const" 1 "local-pure-const1"} } */
/* { dg-final { cleanup-tree-dump "local-pure-const1" } } */

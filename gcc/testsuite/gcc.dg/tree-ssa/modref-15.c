/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-modref1"  } */
int test (int *a)
{
  int i;
  for (i=0; a[i];i++);
  return i+a[i];
}
/* { dg-final { scan-tree-dump "access: Parm 0" "modref1"} } */

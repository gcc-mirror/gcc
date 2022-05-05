/* { dg-do compile } */
/* Disable FRE1 because that for the sake of __builtin_object_size
   will not consider the equality but still valueize 'i', defeating
   the purpose of the check.  */
/* { dg-options "-O -fdump-tree-fre3 -fdisable-tree-fre1" } */

struct S { int a[4]; };

int i;
int bar (struct S *p)
{
  char *q = (char *)p + 4;
  i = 1;
  int *r = &((struct S *)p)->a[i];
  return q == (char *)r;
}
int baz (struct S *p)
{
  i = 1;
  int *r = &((struct S *)p)->a[i];
  char *q = (char *)p + 4;
  return q == (char *)r;
}

/* Verify FRE can handle valueizing &p->a[i] and value-numbering it
   equal to a POINTER_PLUS_EXPR.  */
/* { dg-final { scan-tree-dump-times "return 1;" 2 "fre3" } } */

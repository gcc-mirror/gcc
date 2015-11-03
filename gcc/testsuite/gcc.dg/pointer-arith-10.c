/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

char *foo(char *p, __UINTPTR_TYPE__ i)
{
  return (char *)i + (__UINTPTR_TYPE__)p;
}

/* Check that we use a POINTER_PLUS_EXPR, not something like
   return (char *) ((sizetype) p + (sizetype) i); */
/* { dg-final { scan-tree-dump-not "sizetype.*sizetype" "original" } } */

/* And also that we don't swap the operands.  */
/* { dg-final { scan-tree-dump-not "return p +" "original" } } */

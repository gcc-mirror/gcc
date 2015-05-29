/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1 -W -Wall" } */

int i;
int foo(void)
{
  volatile int *p = (volatile int *)&i;
  return *p + *p;
}

/* We should not convert the cast to a VCE in forwprop1 as we have a
   volatile reference.  */

/* { dg-final { scan-tree-dump-times "VIEW_CONVERT_EXPR" 0 "forwprop1"} } */
/* { dg-final { scan-tree-dump-times "={v}" 2 "forwprop1"} } */

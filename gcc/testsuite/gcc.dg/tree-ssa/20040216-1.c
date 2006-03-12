/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-dse1-details" } */
 
foo(int *z, int *y, int xx)
{
  *z = 1;
  if (xx)
    xx = 20;
  else
   xx = 30;
  *z = 2;
  *z = 3;
  return xx;
}

/* We should convert two COND_EXPRs into straightline code.  */
/* { dg-final { scan-tree-dump-times "Deleted dead store" 2 "dse1"} } */
/* { dg-final { cleanup-tree-dump "dse1" } } */

/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-slp-details" } */

int a[4], b[4];
void foo()
{
  a[0] = b[0] / 7;
  a[1] = b[1] / 7;
  a[2] = b[2] / 7;
  a[3] = b[3] / 7;
}

/* We should cost the original division stmt, not the scalar pattern stmts.  */
/* { dg-final { scan-tree-dump-times " / 7 1 times scalar_stmt costs" 4 "slp2" } } */

/* PR tree-optimization/21031

   Make sure that a != 0 is propagated into the "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1-details" } */

int
foo (int a)
{
  int b = a != 0;
  unsigned char c = b;
  if (c)
    return 1;
  else
    return 0;
}

/* { dg-final { scan-tree-dump-times "Replaced" 2 "forwprop1"} } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */

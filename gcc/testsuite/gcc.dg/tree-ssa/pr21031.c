/* PR tree-optimization/21031

   Make sure that a != 0 is propagated into the "if" statement.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-forwprop1" } */

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

/* { dg-final { scan-tree-dump "if \\(a_\[0-9\]+\\(D\\) != 0\\)" "forwprop1" } } */

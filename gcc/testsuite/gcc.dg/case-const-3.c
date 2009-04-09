/* Test for case labels not integer constant expressions but folding
   to integer constants (used in Linux kernel, PR 39613).  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

extern int i;
void
f (int c)
{
  switch (c)
    {
    case (1 ? 1 : i): /* { dg-error "case label is not an integer constant expression" } */
      ;
    }
}

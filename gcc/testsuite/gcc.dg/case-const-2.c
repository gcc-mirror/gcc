/* Test for case labels not integer constant expressions but folding
   to integer constants (used in Linux kernel, PR 39613).  */
/* { dg-do compile } */
/* { dg-options "-pedantic" } */

extern int i;
void
f (int c)
{
  switch (c)
    {
    case (1 ? 1 : i): /* { dg-warning "case label is not an integer constant expression" } */
      ;
    }
}

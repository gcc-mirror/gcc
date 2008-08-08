/* PR tree-optimization/35899 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a = bar ();	/* { dg-message "note: previous implicit declaration" } */
  return a;
}

void
bar (void)		/* { dg-warning "conflicting types for" } */
{
}

/* PR tree-optimization/35899 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int a = bar ();	/* { dg-error "returning 'void'" } */
  return a;
}

void
bar (void)		/* { dg-warning "conflicting types for" } */
{
}

/* { dg-message "note: previous implicit declaration" "" { target *-*-* } 8 } */

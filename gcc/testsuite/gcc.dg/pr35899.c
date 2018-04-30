/* PR tree-optimization/35899 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -O2" } */

int
foo (void)
{
  int a = bar (); /* { dg-line bar_implicit_decl } */
  return a;
}

void
bar (void)
/* { dg-warning "conflicting types for" "" { target *-*-* } .-1 } */
/* { dg-message "note: previous implicit declaration" "" { target *-*-* } bar_implicit_decl } */
{
}


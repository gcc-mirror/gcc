/* Test that -Wc11-c2x-compat issues a warning (not a pedwarn) about
   `nullptr' in C2X.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

int *
fn (int *p)
{
  p = nullptr; /* { dg-warning "ISO C does not support .nullptr. before C2X" } */
  return p;
}

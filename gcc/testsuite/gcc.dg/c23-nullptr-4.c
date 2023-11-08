/* Test that -Wc11-c23-compat issues a warning (not a pedwarn) about
   `nullptr' in C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors -Wc11-c23-compat" } */

int *
fn (int *p)
{
  p = nullptr; /* { dg-warning "ISO C does not support .nullptr. before C23" } */
  return p;
}

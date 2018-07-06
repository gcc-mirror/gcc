/* PR c++/84590 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=shift" } */

struct S {
  int b;
};

void
fn (void)
{
  struct S c1 = { 1 << -1 }; /* { dg-warning "left shift" } */
}

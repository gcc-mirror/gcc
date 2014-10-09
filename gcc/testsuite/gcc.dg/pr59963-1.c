/* PR c/59963 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -Wdouble-promotion" } */

extern void baz ();
extern void qux (int, ...);

void
foo (float f)
{
  bar (f); /* { dg-warning "8:implicit conversion" } */
  baz (f); /* { dg-warning "8:implicit conversion" } */
  qux (42, f); /* { dg-warning "12:implicit conversion" } */
}

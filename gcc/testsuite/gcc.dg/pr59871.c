/* PR c/59871 */
/* { dg-do compile } */
/* { dg-options "-Wunused" } */

extern int bar ();

void
foo (int *p, int i)
{
  p[0] = (bar (), 1, bar ()); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  p[1] = (1, bar ()); /* { dg-warning "left-hand operand of comma expression has no effect" } */
  bar (), 1, bar (); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  bar (), 1; /* { dg-warning "right-hand operand of comma expression has no effect" } */
  1, bar (); /* { dg-warning "left-hand operand of comma expression has no effect" } */
  (bar (), 1); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  bar (), 5 * i; /* { dg-warning "right-hand operand of comma expression has no effect" } */
  (bar (), 5 * i); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  (bar (), (bar (), (bar (), (bar (), (bar (), (bar (), (bar (), 7))))))); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  bar (), (bar (), (bar (), (bar (), (bar (), (bar (), (bar (), 7)))))); /* { dg-warning "right-hand operand of comma expression has no effect" } */
  bar (), (bar (), (bar (), (bar (), (bar (), (bar (), (7, bar ())))))); /* { dg-warning "left-hand operand of comma expression has no effect" } */
  (bar (), (bar (), (bar (), (bar (), (bar (), (bar (), (7, bar ()))))))); /* { dg-warning "left-hand operand of comma expression has no effect" } */
}

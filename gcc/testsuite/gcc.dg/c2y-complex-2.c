/* Test C2Y complex increment and decrement: warning with -Wc23-c2y-compat.  */
/* { dg-do compile } */
/* { dg-options "-std=c2y -pedantic-errors -Wc23-c2y-compat" } */

_Complex float a;

void
f (void)
{
  a++; /* { dg-warning "does not support" } */
  ++a; /* { dg-warning "does not support" } */
  a--; /* { dg-warning "does not support" } */
  --a; /* { dg-warning "does not support" } */
}

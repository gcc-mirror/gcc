/* Test C2Y complex increment and decrement: disallowed for C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Complex float a;

void
f (void)
{
  a++; /* { dg-error "does not support" } */
  ++a; /* { dg-error "does not support" } */
  a--; /* { dg-error "does not support" } */
  --a; /* { dg-error "does not support" } */
}

/* Test C2Y complex increment and decrement: disallowed for C23 (warning with
   -pedantic).  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic" } */

_Complex float a;

void
f (void)
{
  a++; /* { dg-warning "does not support" } */
  ++a; /* { dg-warning "does not support" } */
  a--; /* { dg-warning "does not support" } */
  --a; /* { dg-warning "does not support" } */
}

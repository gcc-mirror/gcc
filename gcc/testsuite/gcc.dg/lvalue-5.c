/* Test assignment to elements of a string literal is a warning, not
   an error.  PR 27676.  */
/* { dg-do compile } */
/* { dg-options "-pedantic-errors" } */

void
f (void)
{
  "foo"[0] = 0; /* { dg-warning "assignment of read-only location" } */
  "foo"[0]++; /* { dg-warning "increment of read-only location" } */
  "foo"[0]--; /* { dg-warning "decrement of read-only location" } */
  ++"foo"[0]; /* { dg-warning "increment of read-only location" } */
  --"foo"[0]; /* { dg-warning "decrement of read-only location" } */
}

/* PR c++/83059 */
/* { dg-do compile } */

void
foo (int *p, int *q, int *r)
{
  __atomic_compare_exchange (p, q, r, 0, 0, -1);	/* { dg-warning "invalid memory model argument 6" } */
  /* { dg-warning "unknown architecture specifi" "" { target *-*-* } .-1 } */
  /* { dg-warning "failure memory model cannot be stronger than success memory model" "" { target *-*-* } .-2 } */
}

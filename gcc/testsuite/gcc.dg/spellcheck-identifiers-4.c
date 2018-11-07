/* { dg-options "-Wimplicit-function-declaration" } */

extern double sqrt (double);

void test (float pf, float inff)
{
  assert (pf == inff); /* { dg-bogus "sqrt" } */
  /* { dg-warning "implicit declaration of function 'assert'" "" { target *-*-* } .-1 } */
  /* { dg-message "header '<assert.h>'" "" { target *-*-* } .-2 } */
}

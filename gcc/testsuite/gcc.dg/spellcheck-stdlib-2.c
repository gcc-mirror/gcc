/* { dg-options "-Wimplicit-function-declaration" } */

/* Missing <string.h>.  */
void test_string_h (void)
{
  strerror (0); /* { dg-error "implicit declaration of function 'strerror'" } */
  /* { dg-message "'strerror' is defined in header '<string.h>'" "" { target *-*-* } .-1 } */
}

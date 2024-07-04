/* nodiscard attribute tests  */
/* { dg-do compile { target c++20 } } */
/* { dg-options "-O" } */

[[nodiscard(123)]] int check1 (void); /* { dg-error "nodiscard\[^\n\r]*must be a string constant|expected string-literal" } */

void
test (void)
{
  check1 ();
  (void) check1 ();
}

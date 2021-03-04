/* nodiscard attribute tests  */
/* { dg-do compile { target c++2a } } */
/* { dg-options "-O" } */

[[nodiscard("not", "allowed")]] int check1 (void); /* { dg-error "wrong number of arguments.\[^\n\r]*nodiscard" } */

void
test (void)
{
  check1 ();
  (void) check1 ();
}

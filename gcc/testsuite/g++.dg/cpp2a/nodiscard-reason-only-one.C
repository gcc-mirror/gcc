/* nodiscard attribute tests  */
/* { dg-do compile { target c++2a } } */
/* { dg-options "-O -ftrack-macro-expansion=0" } */

[[nodiscard("not", "allowed")]] int check1 (void); /* { dg-error "(?n)wrong number of arguments..*nodiscard" } */

void
test (void)
{
  check1 ();
  (void) check1 ();
}

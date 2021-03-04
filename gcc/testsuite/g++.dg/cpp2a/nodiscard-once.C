/* nodiscard attribute tests  */
/* { dg-do compile { target c++2a } } */
/* { dg-options "-O" } */

[[nodiscard, nodiscard]] int check1 (void); /* { dg-error "nodiscard\[^\n\r]*can appear at most once" } */

void
test (void)
{
  check1 ();			// { dg-warning "nodiscard" }
  (void) check1 ();
}

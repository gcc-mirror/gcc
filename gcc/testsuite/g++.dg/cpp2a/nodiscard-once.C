/* nodiscard attribute tests  */
/* { dg-do compile { target c++20 } } */
/* { dg-options "-O -ftrack-macro-expansion=0" } */

[[nodiscard, nodiscard]] int check1 (void); // { dg-warning "specified multiple times" }

void
test (void)
{
  check1 ();			// { dg-warning "nodiscard" }
  (void) check1 ();
}

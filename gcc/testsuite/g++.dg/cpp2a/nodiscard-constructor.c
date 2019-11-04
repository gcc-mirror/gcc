/* nodiscard attribute tests  */
/* { dg-do compile { target c++2a } } */
/* { dg-options "-O -ftrack-macro-expansion=0" } */

struct A { [[nodiscard("bad constructor")]] A() {} };
struct B { [[nodiscard]] B() {} };

void
test (void)
{
  A{}; /* { dg-warning "(?n)nodiscard.*bad constructor" } */
  B{}; /* { dg-warning "(?n)nodiscard" } */
}

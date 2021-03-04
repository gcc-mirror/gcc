/* nodiscard attribute tests  */
/* { dg-do compile { target c++2a } } */
/* { dg-options "-O" } */

struct A { [[nodiscard("bad constructor")]] A() {} };
struct B { [[nodiscard]] B() {} };

void
test (void)
{
  A{}; /* { dg-warning "nodiscard\[^\n\r]*bad constructor" } */
  B{}; /* { dg-warning "nodiscard" } */
}

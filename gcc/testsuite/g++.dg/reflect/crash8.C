// { dg-do compile { target c++26 } }
// Test using a splice expression in an explicit destructor call
// but without reflection enabled, which ICEd.

struct S { };

void
oy (S s)
{
  s.~typename [:^^S:](); // { dg-error "reflection" }
}

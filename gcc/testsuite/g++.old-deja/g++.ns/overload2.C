namespace A{
  void f();   // ERROR - .*
}

using namespace A;

void f();     // ERROR - .*

void g()
{
  f();        // ERROR - ambiguous, ::f or A::f ?
}

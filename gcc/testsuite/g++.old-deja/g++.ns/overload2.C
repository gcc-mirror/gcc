// { dg-do assemble  }
namespace A{
  void f();   // { dg-error "" } .*
}

using namespace A;

void f();     // { dg-error "" } .*

void g()
{
  f();        // { dg-error "" } ambiguous, ::f or A::f ?
}

// { dg-do assemble  }
namespace A{
  void f();   // { dg-message "note" }
}

using namespace A;

void f();     // { dg-message "note" }

void g()
{
  f();        // { dg-error "ambiguous" } ambiguous, ::f or A::f ?
}

// Build don't link:
namespace A{
  void f();   // ERROR - .*
}

using A::f;
void f();     // ERROR - duplicate declaration


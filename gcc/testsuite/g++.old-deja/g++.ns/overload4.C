// Build don't link:
namespace A{
  void f();  
}

using A::f;
void f();     // ERROR - duplicate declaration


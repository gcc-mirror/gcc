// { dg-do assemble  }
namespace A{
  void f();  
}

using A::f;
void f();     // { dg-error "" } duplicate declaration


// { dg-do assemble  }
namespace A{
  void f(){}        
}

using A::f;

void f(int);
void f(){}            // { dg-error "" } conflict

void g()
{
  f(4);
}

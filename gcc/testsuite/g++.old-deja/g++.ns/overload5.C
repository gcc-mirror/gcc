// Build don't link:
namespace A{
  void f(){}        
}

using A::f;

void f(int);
void f(){}            // ERROR - conflict

void g()
{
  f(4);
}

// To find function pointers in Koenig lookup is ok as long as we only find one.
namespace A{
  void foo();             // ERROR - 
  struct X{};
  void (*bar)(X*)=0;
}
using A::X;

void (*foo)(X*)=0;        // ERROR - 

void g()
{
  foo(new X);            // ERROR - both objects and functions found
  bar(new X);            // ok
}

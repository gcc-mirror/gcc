// { dg-do assemble  }
// To find function pointers in Koenig lookup is ok as long as we only find one.
namespace A{
  void foo();             // { dg-error "" } 
  struct X{};
  void (*bar)(X*)=0;
}
using A::X;

void (*foo)(X*)=0;        // { dg-error "" } 

void g()
{
  foo(new X);            // { dg-error "" } both objects and functions found
  bar(new X);            // ok
}

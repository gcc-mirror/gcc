// { dg-do assemble  }
// To find function pointers in Koenig lookup is ok as long as we only find one.
namespace A{
  void foo();             
  struct X{};
  void (*bar)(X*)=0;
}
using A::X;

void (*foo)(X*)=0;        

void g()
{
  foo(new X);            // ok -- DR 218 says that we find the global
			 // foo variable first, and therefore do not
			 // perform argument-dependent lookup.
  bar(new X);            // ok
}

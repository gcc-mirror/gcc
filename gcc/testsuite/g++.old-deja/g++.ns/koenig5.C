// { dg-do assemble  }
// Function pointers are ignored in Koenig lookup. (DR 218)
namespace A{
  void foo();             
  struct X{};
  void (*bar)(X*)=0;		// { dg-message "A::bar" }
}
using A::X;

void (*foo)(X*)=0;        

void g()
{
  foo(new X);            // ok -- DR 218 says that we find the global
			 // foo variable first, and therefore do not
			 // perform argument-dependent lookup.
  bar(new X);            // { dg-error "3:'bar' was not declared in this scope; did you mean 'A::bar'" }
}

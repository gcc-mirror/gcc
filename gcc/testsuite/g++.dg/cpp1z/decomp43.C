// PR c++/85194
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int i; };

A x[2];

void
foo ()
{
  for (auto [i] = A () : x)	// { dg-error "initializer in range-based 'for' loop" }
    ;				// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}

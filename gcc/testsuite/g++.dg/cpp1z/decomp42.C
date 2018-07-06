// PR c++/85210
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int i; };

template <int>
void
foo (int j)
{
  auto [j] = A{j};	// { dg-error "shadows a parameter" }
}			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }

void
bar ()
{
  foo<0> (0);
}

// PR c++/85209
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <int>
void
foo ()
{
  auto [a] = []{};	// { dg-error "cannot decompose lambda closure type" }
}			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }

void
bar ()
{
  foo<0> ();
}

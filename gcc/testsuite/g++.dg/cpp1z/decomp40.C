// PR c++/85209
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct S { int a; } s;

template <int>
void
foo ()
{
  auto [a] = []{ return s; } ();	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
};

void
bar ()
{
  foo<0> ();
}

// PR c++/84469
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A {
  template <typename T>
  void bar () const {}
  template <typename T>
  void baz () const {}
};
struct B { A a; };

template <typename>
void
foo ()
{
  A a[1][1];
  for (auto const& [b]: a)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    b.bar<int> ();
  B c;
  auto const& [d] = c;		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  d.baz<double> ();
}

int
main ()
{
  foo<int> ();
}

// PR c++/79372
// { dg-do compile { target c++11 } }
// { dg-options "" }

template <typename T>
struct S
{
  enum E { A };
  void f () { auto [x] = 0; x++; }	// { dg-error "cannot decompose non-array non-class type" }
					// { dg-warning "decomposition declaration only available with" "" { target c++14_down } .-1 }
  void g (T t) { auto [y] = t; y++; }	// { dg-error "cannot decompose non-array non-class type" }
};					// { dg-warning "decomposition declaration only available with" "" { target c++14_down } .-1 }

int
main ()
{
  S <int> s;
  s.f ();
  s.g (5);
}

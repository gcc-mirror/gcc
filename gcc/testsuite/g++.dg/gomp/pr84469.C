// PR c++/84469
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A {
  template <typename T>
  void bar () const {}
};

template <typename>
void
foo ()
{
  A a[1][1];
  #pragma omp for
  for (auto const& [b]: a)	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
    b.bar<int> ();
}

int
main ()
{
  foo<int> ();
}
